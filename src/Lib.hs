{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Lib where

import           Control.Arrow
import           Control.Monad.State
import           Data.Bool                      ( bool )
import           Data.Char
import           Data.Default
import           Data.Functor                   ( (<&>) )
import           Data.List
import           Data.Maybe

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------
-- | Apply a function to the first element of a pair
mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

-- | Apply a function to the state of a State monad instance
update :: (s -> s) -> State s ()
update f = state $ \s -> ((), f s)

------------------------------------------------------------------------------
-- typeclass Parseable and parsing
------------------------------------------------------------------------------
type Parse a = (Maybe a, String)

class Parseable a where
    parse :: String -> Parse a
    default parse :: (Read a) => String -> Parse a
    parse str = case reads str of
        [ ( x, xs ) ] -> ( Just x, xs )
        _ -> ( Nothing, str )

instance Parseable Int

instance Parseable Char where
  parse ""       = (Nothing, "")
  parse (x : xs) = (Just x, xs)

data Address = LineNum Int | LastLine
    deriving ( Show, Eq, Ord )

data OptAddr2 = NoAddr | Addr1 Address | Addr2 Address Address
    deriving ( Show, Eq, Ord )

type Function = Char

type Command = (OptAddr2, Function, String)

instance Parseable Address where
  parse ('$' : xs) = (Just LastLine, xs)
  parse xs         = fmap LineNum `mapFst` (parse xs :: Parse Int)

instance Parseable OptAddr2 where
  parse xs = case parse xs :: Parse Address of
    (Nothing, xs) -> (Just NoAddr, xs)
    (Just a1, xs) -> case parse (eatCharAndBlanks ',' xs) :: Parse Address of
      (Nothing, xs) -> (Just $ Addr1 a1, xs)
      (Just a2, xs) -> (Just $ Addr2 a1 a2, xs)

instance Parseable Command where
  parse xs =
    let (Just addr, xs' ) = parse xs :: Parse OptAddr2
        (func     , xs'') = parse (eatSpace xs') :: Parse Char
    in  (fmap (addr, , xs'') func, xs'')

parseScript :: String -> [Command]
parseScript xs = maybeToList $ fst $ parse $ eatSpaceOrSemCol xs

------------------------------------------------------------------------------
-- Parsing utilities
------------------------------------------------------------------------------
eat :: (Char -> Bool) -> String -> String
eat f (x : xs) | f x = eat f xs
eat _ xs             = xs

eatChar :: Char -> String -> String
eatChar a = eat (a ==)

eatSpace :: String -> String
eatSpace = eat isSpace

-- Eat a char and any blank chars in front and after it
eatCharAndBlanks :: Char -> String -> String
eatCharAndBlanks a = eatSpace . eatChar a . eatSpace

eatSpaceOrSemCol :: String -> String
eatSpaceOrSemCol = eat $ \x -> isSpace x || x == ';'

------------------------------------------------------------------------------
-- Execution
------------------------------------------------------------------------------
type SedState = State SedStateData

-- | The state of the stream editor
-- Saved between cycles
data SedStateData
    = SedStateData { patternSpace :: String, holdSpace :: String, lineNum
          :: Int, isLastLine :: Bool, insideRanges :: [ ( Address, Address ) ] }
    deriving ( Show )

instance Default SedStateData where
  def = SedStateData { patternSpace = ""
                     , holdSpace    = ""
                     , lineNum      = 0
                     , isLastLine   = False
                     , insideRanges = []
                     }

-- | Clear the pattern space
clearPs :: SedState ()
clearPs = update $ \s -> s { patternSpace = "" }

-- | The main `sed` function. Takes a script and an input, parses the script,
-- splits the input into lines, and evaluates the whole thing, returning
-- the output string
execute :: String -> String -> String
execute script input =
  evalState (executeSed (parseScript script) (lines input, "")) def

executeSed :: [Command] -> ([String], String) -> SedState String
executeSed script ([]   , output) = return output
executeSed script (input, output) = do
  let (line : input') = input
  update $ \s -> s { patternSpace = line
                   , lineNum      = lineNum s + 1
                   , isLastLine   = null input'
                   }
  dOutput <- doCycle script
  executeSed script (input', output ++ dOutput)

-- | Runs A full cycle of commands given a `sed` state.
-- Returns the updates  state, and a string to be appended to the output
doCycle :: [Command] -> SedState String
doCycle (c : cmds) = do
  res <- applyCommand c
  case res of
    Continue               -> doCycle cmds
    NextCycle              -> return ""
    WriteAndContinue  text -> (text ++) <$> doCycle cmds
    WriteAndNextCycle text -> return text
-- Base case: After running all commands, write pattern space to output
doCycle [] = gets (patternSpace >>> (++ ['\n']))

------------------------------------------------------------------------------
-- Address checking
------------------------------------------------------------------------------
-- | Check whether a single address selects the current pattern
checkAddr1 :: Address -> SedState Bool
checkAddr1 (LineNum a) = (== a) <$> gets lineNum
checkAddr1 LastLine    = gets isLastLine

data AddressCheck = ACNone | ACOutside | ACOne | ACFirst | ACBetween | ACLast
    deriving ( Eq, Show, Ord )

-- | Check whether a address (or range of addresses) selects the current pattern
checkAddr :: OptAddr2 -> SedState AddressCheck
checkAddr NoAddr        = return ACNone
checkAddr (Addr1 a    ) = checkAddr1 a <&> bool ACOutside ACOne
checkAddr (Addr2 a1 a2) = do
  n <- gets lineNum
  case (a2, n) of
      -- A special case: If the second address is a number less than or equal to the
      -- line number first selected, only one line shall be selected.
    (LineNum l, n) | l < n -> checkAddr (Addr1 a1)
    _                      -> do
      inside <- elem (a1, a2) <$> gets insideRanges
      b      <- checkAddr1 (if inside then a2 else a1)
      case (inside, b) of
        (True, True) ->
          state $ \s ->
            (ACLast, s { insideRanges = delete (a1, a2) $ insideRanges s })
        (True , False) -> return ACBetween
        (False, True ) -> state
          $ \s -> (ACFirst, s { insideRanges = (a1, a2) : insideRanges s })
        (False, False) -> return ACOutside

-- | Given an address range, a state and a function, check the address and return
-- `(Continue, state)` if the address does not select the current state, or the
-- result of `func state'` if it does.
--
-- Note: NoAddr selects anything!
ifAddrSelects :: OptAddr2 -> SedState CommandResult -> SedState CommandResult
ifAddrSelects addr func =
  checkAddr addr <&> acSelects >>= bool (return Continue) func

-- | Whether an address check selected the line
acSelects :: AddressCheck -> Bool
acSelects ACOutside = False
acSelects _         = True

-- | Whether an address check is the last match of a range
-- is True for ACNone, ACOne, and ACLast, False for all others
acIsLast :: AddressCheck -> Bool
acIsLast ACNone = True
acIsLast ACOne  = True
acIsLast ACLast = True
acIsLast _      = False

------------------------------------------------------------------------------
-- Command implementations
------------------------------------------------------------------------------
-- | The result of applying a command.
--  Tells `doCycle` what to do
data CommandResult
    = Continue | NextCycle | WriteAndContinue String | WriteAndNextCycle String

-- | Apply a command
applyCommand :: Command -> SedState CommandResult

-- Delete the pattern space and start the next cycle.
applyCommand (a, 'd', "") =
  ifAddrSelects a (state $ \s -> (NextCycle, s { patternSpace = "" }))
-- Write the pattern space to standard output.
applyCommand (a, 'p', "") =
  ifAddrSelects a $ gets (WriteAndContinue . (++ ['\n']) . patternSpace)
-- Delete the pattern space. With a 0 or 1 address or at the end of a
-- 2-address range, place text on the output and start the next cycle.
--
-- Note: This wording leaves a few questions:
--  - Does it start a new cycle for each line inside a 2-address range?
--    GNU sed does, so we do too. It seems to make the most sense.
--  - Newline after `text` on output? GNU sed does, and it makese sense.
applyCommand (a, 'c', text) = do
  ac <- checkAddr a
  if acSelects ac
    then if acIsLast ac
      then return (WriteAndNextCycle $ unescapeTextArg text ++ ['\n'])
      else state $ \s -> (NextCycle, s { patternSpace = "" })
    else return Continue
-- Exchange the contents of the pattern and hold spaces.
applyCommand (a, 'x', "") = ifAddrSelects a $ state $ \s ->
  (Continue, s { patternSpace = holdSpace s, holdSpace = patternSpace s })

-- | unescape the `text` argument as specified in the spec
--
-- Spec: The argument text shall consist of one or more lines. Each embedded
-- <newline> in the text shall be preceded by a <backslash>. Other <backslash>
-- characters in text shall be removed, and the following character shall be
-- treated literally.
--
-- Also, it starts with a backslash followed by a newline. GNU sed allows this
-- newline to be omitted, we currently don't.
unescapeTextArg :: String -> String
unescapeTextArg ('\\' : '\n' : xs) = unescape xs
 where
  unescape ('\\' : x : xs) = x : unescape xs
  unescape (x        : xs) = x : unescape xs
  unescape []              = []

{-# LANGUAGE BlockArguments #-}
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
-- Monad Parser & typeclass Parseable
------------------------------------------------------------------------------

newtype Parser a = Parser { runParser :: String -> (Maybe a, String) }

instance Functor Parser where
  fmap f p = Parser $ \s -> case runParser p s of
    (Nothing, xs) -> (Nothing, xs)
    (Just a , xs) -> (Just $ f a, xs)

instance Applicative Parser where
  pure a = Parser $ \s -> (Just a, s)
  pf <*> p = Parser $ \s -> case runParser pf s of
    (Nothing, xs) -> (Nothing, xs)
    (Just f , xs) -> case runParser p xs of
      (Nothing, xs') -> (Nothing, xs')
      (Just a , xs') -> (Just (f a), xs')

instance Monad Parser where
  return = pure
  p >>= f = Parser $ \s -> case runParser p s of
    (Nothing, xs) -> (Nothing, xs)
    (Just a , xs) -> runParser (f a) xs

evalParser :: Parser a -> String -> Maybe a
evalParser p = fst . runParser p

execParser :: Parser a -> String -> String
execParser p = snd . runParser p

class Parseable a where
    parse :: Parser a
    parser :: String -> (Maybe a, String)
    parse = Parser $ parser
    parser = runParser parse
    {-# MINIMAL parse | parser #-}

parseRead :: (Read a) => Parser a
parseRead = Parser $ \s -> case reads s of
  [(x, xs)] -> (Just x, xs)
  _         -> (Nothing, s)

instance Parseable Int where
  parse = parseRead

instance Parseable Char where
  parser ""       = (Nothing, "")
  parser (x : xs) = (Just x, xs)

------------------------------------------------------------------------------
-- Parsing utilities
------------------------------------------------------------------------------

opt :: Parser a -> Parser (Maybe a)
opt p = Parser $ \s -> case runParser p s of
  (Nothing, xs) -> (Just Nothing, xs)
  (Just a , xs) -> (Just $ Just a, xs)

putRemaining :: String -> Parser ()
putRemaining s = Parser $ \xs -> (Just (), s)

eat :: (Char -> Bool) -> Parser String
eat f = Parser $ parser ""
 where
  parser s (x : xs) | f x = parser (s ++ [x]) xs
  parser s xs             = (Just s, xs)

eatChar :: Char -> Parser String
eatChar a = eat (a ==)

eatSpace :: Parser String
eatSpace = eat isSpace

-- | Eat a char and any blank chars in front and after it
eatCharAndBlanks :: Char -> Parser String
eatCharAndBlanks a = eatSpace >> eatChar a >> eatSpace

eatSpaceOrSemCol :: Parser String
eatSpaceOrSemCol = eat $ \x -> isSpace x || x == ';'

-- | Eat until a character matches a predicate
--
-- The character will be included in neither the result or the remaining string
eatUntil :: (Char -> Bool) -> Parser String
eatUntil f = Parser (parser "")
 where
  parser o (x : xs) | f x = (Just o, xs)
  parser o (x : xs)       = parser (o ++ [x]) xs
  parser o []             = (Just o, "")

-- | Eat until the given character is met
--
-- The character will be included in neither the result or the remaining string
eatUntilChar :: Char -> Parser String
eatUntilChar c = eatUntil (== c)

-- | Eat until an unescaped character matches a predicate
--
-- The character will be included in neither the result or the remaining string
eatUntilUnescaped :: (Char -> Bool) -> Parser String
eatUntilUnescaped f = Parser (parser "")
 where
  parser o ('\\' : x : xs) = parser (o ++ ['\\', x]) xs
  parser o (x : xs) | f x  = (Just o, xs)
  parser o (x : xs)        = parser (o ++ [x]) xs
  parser o []              = (Just o, "")

-- | Eat until the given character is met unescaped
--
-- The character will be included in neither the result or the remaining string
eatUntilUnescapedChar :: Char -> Parser String
eatUntilUnescapedChar c = eatUntilUnescaped (== c)

-- | Keep parsing a's until one fails, returning a list of all parsed 'a's
parseSequence :: Parser a -> Parser [a]
parseSequence p = Parser $ parser []
 where
  parser r s = case (runParser p s) of
    (Nothing, xs) -> (Just r, xs)
    (Just a , xs) -> parser (r ++ [a]) xs

------------------------------------------------------------------------------
-- Parseable Data types and implementations
------------------------------------------------------------------------------

data Address = LineNum Int | LastLine deriving (Show, Eq, Ord)
data OptAddr2 = NoAddr | Addr1 Address | Addr2 Address Address
  deriving (Show, Eq, Ord)
type Function = Char
type Command = (OptAddr2, Function, String)

-- | Whether a function can be foll
canTerminateWithSemicolon :: Function -> Bool
canTerminateWithSemicolon f =
  not $ f `elem` ['{', 'a', 'b', 'c', 'i', 'r', 't', 'w', ':', '#']

instance Parseable Address where
  parser ('$' : xs) = (Just LastLine, xs)
  parser xs         = runParser (fmap LineNum (parse :: Parser Int)) xs

instance Parseable OptAddr2 where
  parse =
    opt (parse :: Parser Address)
      >>= (maybe (return NoAddr) $ \a1 ->
            (eatCharAndBlanks ',')
              >>  opt (parse :: Parser Address)
              >>= (maybe (return $ Addr1 a1) $ \a2 -> return $ Addr2 a1 a2)
          )

instance Parseable Command where
  parse = do
    addr <- parse :: Parser OptAddr2
    eatSpace
    func <- parse :: Parser Char
    arg  <- eatUntilUnescaped
      (\c -> c == '\n' || (canTerminateWithSemicolon func && c == ';'))
    return (addr, func, arg)

parseScript :: String -> [Command]
parseScript xs =
  fromMaybe [] $ evalParser (parseSequence $ eatSpaceOrSemCol >> parse) xs

------------------------------------------------------------------------------
-- Execution
------------------------------------------------------------------------------
type SedState = State SedStateData

-- | The state of the stream editor
-- Saved between cycles
data SedStateData = SedStateData {
  patternSpace :: String,
  holdSpace :: String,
  lineNum :: Int,
  isLastLine :: Bool,
  insideRanges :: [(Address, Address)]
} deriving (Show)

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

-- | Check whether a address (or range of addresses) selects the current
-- pattern
checkAddr :: OptAddr2 -> SedState AddressCheck
checkAddr NoAddr        = return ACNone
checkAddr (Addr1 a    ) = checkAddr1 a <&> bool ACOutside ACOne
checkAddr (Addr2 a1 a2) = do
  n <- gets lineNum
  case (a2, n) of
      -- A special case: If the second address is a number less than or equal
      -- to the line number first selected, only one line shall be selected.
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

-- | Given an address range, a state and a function, check the address and
-- return `(Continue, state)` if the address does not select the current state,
-- or the result of `func state'` if it does.
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

-- Replace the contents of the pattern space by the contents of the hold space.
applyCommand (a, 'g', "") =
  ifAddrSelects a $ state $ \s -> (Continue, s { patternSpace = holdSpace s })

-- Append to the pattern space a <newline> followed by the contents of the hold space.
applyCommand (a, 'G', "") = ifAddrSelects a $ state $ \s ->
  (Continue, s { patternSpace = (patternSpace s) ++ ['\n'] ++ holdSpace s })

-- Replace the contents of the hold space by the contents of the pattern space.
applyCommand (a, 'h', "") =
  ifAddrSelects a $ state $ \s -> (Continue, s { holdSpace = patternSpace s })

-- Append to the hold space a <newline> followed by the contents of the pattern space.
applyCommand (a, 'H', "") = ifAddrSelects a $ state $ \s ->
  (Continue, s { holdSpace = (holdSpace s) ++ ['\n'] ++ patternSpace s })

-- Write the pattern space, up to the first <newline>, to standard output.
applyCommand (a, 'P', "") = ifAddrSelects a $ gets
  (   patternSpace
  >>> ((fromMaybe "") . evalParser (eatUntilChar '\n'))
  >>> (++ ['\n'])
  >>> WriteAndContinue
  )

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

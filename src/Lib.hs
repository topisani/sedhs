{-# LANGUAGE TupleSections, DefaultSignatures, FlexibleInstances #-}
module Lib where

import           Data.Char
import           Data.Maybe
import           Data.List
import           Control.Monad.State

eatSpace :: String -> String
eatSpace (x : xs) | isSpace x = eatSpace xs
eatSpace xs                   = xs

eatChar :: Char -> String -> String
eatChar a (x : xs) | x == a = xs
eatChar _ xs                = xs

-- Eat a char and any blank chars in front and after it
eatCharAndBlanks :: Char -> String -> String
eatCharAndBlanks a = eatSpace . eatChar a . eatSpace

eatSpaceOrSemCol :: String -> String
eatSpaceOrSemCol (x : xs) | isSpace x || x == ';' = eatSpaceOrSemCol xs
eatSpaceOrSemCol xs                               = xs

nextLine :: String -> (String, String)
nextLine str = getLineImpl ("", str)
 where
  getLineImpl (line, '\n' : xs) = (line, xs)
  getLineImpl (line, x : xs   ) = getLineImpl (line ++ [x], xs)
  getLineImpl (line, []       ) = (line, [])

type Parse a = (Maybe a, String)
class Parseable a where
  parse :: String -> Parse a
  default parse :: (Read a) => String -> Parse a
  parse str = case reads str of
                  [(x, xs)] -> (Just x, xs)
                  _         -> (Nothing, str)

instance Parseable Int
instance Parseable Char where
  parse ""       = (Nothing, "")
  parse (x : xs) = (Just x, xs)

type Address = Int
data OptAddr2 = NoAddr | Addr1 Address | Addr2 Address Address deriving (Show, Eq, Ord)
type Function = Char
type Command = (OptAddr2, Function, String)

instance Parseable OptAddr2 where
  parse xs = case parse xs :: Parse Int of
    (Nothing, xs) -> (Just NoAddr, xs)
    (Just a1, xs) -> case parse (eatCharAndBlanks ',' xs) :: Parse Int of
      (Nothing, xs) -> (Just $ Addr1 a1, xs)
      (Just a2, xs) -> (Just $ Addr2 a1 a2, xs)

instance Parseable Command where
  parse xs =
    let (Just addr, xs' ) = parse xs :: Parse OptAddr2
        (func     , xs'') = parse (eatSpace xs') :: Parse Char
    in  (fmap (addr, , xs'') func, xs'')


data StreamEditor = StreamEditor {
  patternSpace :: String,
  holdSpace :: String,
  lineNum :: Int,
  insideRanges :: [(Address, Address)]
} deriving (Show)

data CommandResult = Continue | NextCycle

defaultSed :: StreamEditor
defaultSed = StreamEditor { patternSpace = ""
                          , holdSpace    = ""
                          , lineNum      = 0
                          , insideRanges = []
                          }

incState state space =
  state { patternSpace = space, lineNum = lineNum state + 1 }

update :: (s -> s) -> State s ()
update f = state $ \s -> ((), f s)

execute :: String -> String -> String
execute script input = executeSed (parseScript script) (input, defaultSed, "")

parseScript :: String -> [Command]
parseScript xs = maybeToList $ fst $ parse $ eatSpaceOrSemCol xs

executeSed :: [Command] -> (String, StreamEditor, String) -> String
executeSed script ("", state, output) = output
executeSed script (input, state, output) =
  let (line, input')     = nextLine input
      state'             = incState state line
      (state'', dOutput) = doCycle script state'
      output'            = output ++ dOutput
  in  executeSed script (input', state'', output')

-- |Runs A full cycle of commands given a `sed` state.
-- Returns the updates  state, and a string to be appended to the output
doCycle :: [Command] -> StreamEditor -> (StreamEditor, String)
doCycle (c : cmds) state =
  let (res, state') = runState (applyCommand c) state
  in  case res of
        Continue  -> doCycle cmds state'
        NextCycle -> (state', "")
-- Base case: After running all commands, write pattern space to output
doCycle [] state = (state, patternSpace state ++ ['\n'])

-- Check whether a single address selects the current pattern
checkAddr1 :: Address -> State StreamEditor Bool
checkAddr1 a = (== a) <$> gets lineNum

data AddressCheck = ACNone | ACOutside | ACFirst | ACLast | ACBetween
-- Check whether a address (or range of addresses) selects the current pattern
checkAddr :: OptAddr2 -> State StreamEditor AddressCheck
checkAddr NoAddr    = return ACNone
checkAddr (Addr1 a) = do
  b <- checkAddr1 a
  return (if b then ACFirst else ACOutside)

checkAddr (Addr2 a1 a2) = do
  n <- gets lineNum
  if a2 < n
    -- A special case: If the second address is a number less than or equal to the
    -- line number first selected, only one line shall be selected.
    then checkAddr (Addr1 a1)
    else do
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

-- Given an address range, a state and a function, check the address and return
-- `(Continue, state)` if the address does not select the current state, or the
-- result of `func state'` if it does.
--
-- Note: NoAddr selects anything!
ifAddrSelects
  :: OptAddr2
  -> State StreamEditor CommandResult
  -> State StreamEditor CommandResult
ifAddrSelects addr func = do
  ac <- checkAddr addr
  case ac of
    ACOutside -> return Continue
    _         -> func

applyCommand :: Command -> State StreamEditor CommandResult
applyCommand (a, 'd', _) =
  ifAddrSelects a (state $ \s -> (NextCycle, s { patternSpace = "" }))

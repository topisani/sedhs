{-# LANGUAGE TupleSections, DefaultSignatures, FlexibleInstances #-}
module Lib where

import           Data.Char
import           Data.Maybe
import           Data.List

eatSpace :: String -> String
eatSpace (x : xs) | isSpace x = eatSpace xs
eatSpace xs                   = xs

eatChar :: Char -> String -> String
eatChar a (x : xs) | x == a = xs
eatChar _ xs = xs

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
    let (Just addr, xs') = parse xs :: Parse OptAddr2
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
  let (res, state') = applyCommand c state
  in  case res of
        Continue  -> doCycle cmds state'
        NextCycle -> (state', "")
-- Base case: After running all commands, write pattern space to output
doCycle [] state = (state, patternSpace state ++ ['\n'])

-- Check whether a single address selects the current pattern
checkAddr1 :: Address -> StreamEditor -> Bool
checkAddr1 a state | lineNum state == a = True
checkAddr1 _ state                      = False

data AddressCheck = ACNone | ACOutside | ACFirst | ACLast | ACBetween
-- Check whether a address (or range of addresses) selects the current pattern
checkAddr :: OptAddr2 -> StreamEditor -> (AddressCheck, StreamEditor)
checkAddr NoAddr state                         = (ACNone, state)
checkAddr (Addr1 a) state | checkAddr1 a state = (ACFirst, state)
checkAddr (Addr1 _    ) state                  = (ACOutside, state)

-- A special case: If the second address is a number less than or equal to the
-- line number first selected, only one line shall be selected.
checkAddr (Addr2 a1 a2) state | a2 < lineNum state = checkAddr (Addr1 a1) state

checkAddr (Addr2 a1 a2) state                  = case (inside, check) of
  (False, False) -> (ACOutside, state)
  (False, True ) -> (ACFirst, stateAdd)
  (True , False) -> (ACBetween, state)
  (True , True ) -> (ACLast, stateDel)
 where
  inside   = (a1, a2) `elem` insideRanges state
  nextA    = if inside then a2 else a1
  check    = checkAddr1 nextA state
  stateAdd = state { insideRanges = (a1, a2) : insideRanges state }
  stateDel = state { insideRanges = delete (a1, a2) (insideRanges state) }

-- Given an address range, a state and a function, check the address and return
-- `(Continue, state)` if the address does not select the current state, or the
-- result of `func state'` if it does.
--
-- Note: NoAddr selects anything!
ifAddrSelects
  :: OptAddr2
  -> StreamEditor
  -> (StreamEditor -> (CommandResult, StreamEditor))
  -> (CommandResult, StreamEditor)
ifAddrSelects addr state func =
  let (ac, state') = checkAddr addr state
      inside       = case ac of
        ACOutside -> False
        _         -> True
  in  if inside then func state' else (Continue, state')

applyCommand :: Command -> StreamEditor -> (CommandResult, StreamEditor)
applyCommand (a, 'd', _) state =
  ifAddrSelects a state (\state' -> (NextCycle, state' { patternSpace = "" }))

{-# LANGUAGE TupleSections, DefaultSignatures, FlexibleInstances #-}
module Lib where

import           Data.Char
import           Data.Maybe

eatSpace :: String -> String
eatSpace (x : xs) | isSpace x = eatSpace xs
eatSpace xs                   = xs

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
data OptAddr2 = NoAddr | Addr1 Address | Addr2 Address Address deriving (Show, Eq)
type Function = Char
type Command = (OptAddr2, Function, String)

instance Parseable OptAddr2 where
  parse xs = case parse xs :: Parse Int of
    (Nothing, xs) -> (Just NoAddr, xs)
    (Just a1, xs) -> case parse (eatSpace xs) :: Parse Int of
      (Nothing, xs) -> (Just $ Addr1 a1, xs)
      (Just a2, xs) -> (Just $ Addr2 a1 a2, xs)

instance Parseable Command where
  parse xs =
    let (Just addr, ys) = parse xs :: Parse OptAddr2
        (func     , zs) = parse ys :: Parse Char
    in  (fmap (addr, , zs) func, zs)


data StreamEditor = StreamEditor {
  patternSpace :: String,
  lineNum :: Int
} deriving (Show)

data CommandResult = Continue | NextCycle

defaultSed :: StreamEditor
defaultSed = StreamEditor { patternSpace = "", lineNum = 0 }

incState state space = StreamEditor { patternSpace = space, lineNum = lineNum state + 1}

execute :: String -> String -> String
execute input script = executeSed (parseScript script) (input, defaultSed, "")

parseScript :: String -> [Command]
parseScript xs = maybeToList $ fst $ parse $ eatSpaceOrSemCol xs

executeSed :: [Command] -> (String, StreamEditor, String) -> String
executeSed script (""   , state, output) = output
executeSed script (input, state, output) =
  let (line, input') = nextLine input
      state' = incState state line
      output' = output ++ doCycle script state'
  in executeSed script (input', state', output')

doCycle :: [Command] -> StreamEditor -> String
doCycle (c : cmds) state =
  let (res, state') = applyCommand c state
  in  case res of
        Continue  -> doCycle cmds state'
        NextCycle -> ""
doCycle [] state = patternSpace state ++ ['\n']

applyCommand :: Command -> StreamEditor -> (CommandResult, StreamEditor)
applyCommand (Addr1 a, 'd', _) state | lineNum state == a = (NextCycle, state { patternSpace = "" })
applyCommand (Addr1 _, 'd', _) state = (Continue, state)
applyCommand (_, 'd', _) state = (NextCycle, state { patternSpace = "" })

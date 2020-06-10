{-# LANGUAGE TupleSections, DefaultSignatures, FlexibleInstances #-}
module Lib where

import           Data.Char

type Parse a = (Maybe a, String)
class Parseable a where
  parse :: String -> Parse a
  default parse :: (Read a) => String -> Parse a
  parse str = case reads str of
                  [(x, xs)] -> (Just x, xs)
                  _         -> (Nothing, str)

instance Parseable Int
instance Parseable Char where
  parse "" = (Nothing, "")
  parse (x:xs) = (Just x, xs)

type Address = Int
data OptAddr2 = NoAddr | Addr1 Address | Addr2 Address Address deriving (Show, Eq)
type Function = Char
type Command = (OptAddr2, Function, String)

eatSpace :: String -> String
eatSpace (x : xs) | isSpace x = eatSpace xs
eatSpace xs                   = xs

eatSpaceOrSemCol :: String -> String
eatSpaceOrSemCol (x : xs) | isSpace x || x == ';' = eatSpaceOrSemCol xs
eatSpaceOrSemCol xs                               = xs

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

execute :: String -> String -> String
execute input script = applyCommand input (parseScript script)

parseScript :: String -> Maybe Command
parseScript xs = fst $ parse $ eatSpaceOrSemCol xs

applyCommand :: String -> Maybe Command -> String
applyCommand input Nothing    = input ++ ['\n']
applyCommand input (Just cmd) = case cmd of
  (_, 'd', _) -> ""

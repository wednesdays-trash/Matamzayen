module Matamzayen.Parser where

import Data.List.Split

funcFromString :: Floating a => String -> (a -> a)
funcFromString str =
  case str of
    "inc" -> (1 +)
    "double" -> (2 *)
    "sqrt" -> sqrt

createComposition :: Floating a => [String] -> (a -> a)
createComposition = foldr1 (.) . map funcFromString

parse :: Floating a => String -> [a -> a]
parse input = map createComposition funcs
  where
    funcs = map (splitOn ".") $ splitOn "^" (tail input)

module Matamzayen.Parser where

import Data.List.Split
import Debug.Trace

mainFuncIdentifier = '@'
funcSeparator = "."
funcIdentifier = "^"

funcFromString :: Floating a => String -> (a -> a)
funcFromString str =
  case str of
    "inc" -> (+ 1)
    "dec" -> subtract 1
    "double" -> (* 2)
    "sqrt" -> sqrt

createComposition :: Floating a => String -> (a -> a)
createComposition = foldr1 (.) . map funcFromString . splitOn funcSeparator

getMainFunc funcsStrings =
  createComposition $ tail $ head $ filter (elem mainFuncIdentifier) funcsStrings

parse :: Floating a => String -> a -> a
parse program = getMainFunc funcsStrings
  where
    funcsStrings = splitOn funcIdentifier (tail program)
    funcs = map createComposition (filter (notElem mainFuncIdentifier) funcsStrings)

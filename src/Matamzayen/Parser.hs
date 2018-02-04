module Matamzayen.Parser where

import Data.List.Split
import Debug.Trace

mainFuncIdentifier = '@'
funcSeparator = "."
funcIdentifier = "^"

funcFromString :: Floating a => String -> (a -> a)
funcFromString str =
    case str of
        "inc"    -> (+ 1)
        "dec"    -> subtract 1
        "double" -> (* 2)
        "sqrt"   -> sqrt

createComposition :: Floating a => String -> (a -> a)
createComposition = foldr1 (.) . map funcFromString . splitOn funcSeparator

getMainFunc :: Floating a => [String] -> (a -> a)
getMainFunc funcsStrings =
    let str = tail . head . filter (elem mainFuncIdentifier) $ funcsStrings
    in createComposition str

parse :: Floating a => String -> a -> a
parse program = getMainFunc funcsStrings
  where
    funcsStrings = splitOn funcIdentifier (tail program)
    funcsStringWithoutMain = filter (notElem mainFuncIdentifier) funcsStrings
    funcs = map createComposition funcsStringWithoutMain

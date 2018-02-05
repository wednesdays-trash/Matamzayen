module Matamzayen.Parser where

import Data.List.Split
import Matamzayen.BaseFuncs

mainFuncIdentifier = '@'
funcSeparator = "."
funcIdentifier = "^"

parse :: Floating a => String -> a -> a
parse program = getMainFunc funcsStrings
  where
    funcsStrings :: [String]
    funcsStrings = tail $ splitOn funcIdentifier program

    funcsStringsWithoutMain :: [String]
    funcsStringsWithoutMain = filter (notElem mainFuncIdentifier) funcsStrings

    funcs :: Floating a => [a -> a]
    funcs = map createComposition funcsStringsWithoutMain

    funcFromString :: Floating a => String -> (a -> a)
    funcFromString str = getFuncByString str funcs

    createComposition :: Floating a => String -> (a -> a)
    createComposition = foldr1 (.) . map funcFromString . splitOn funcSeparator
    
    getMainFunc :: Floating a => [String] -> (a -> a)
    getMainFunc funcsStrings =
        let str = tail . head . filter (elem mainFuncIdentifier) $ funcsStrings
        in createComposition str

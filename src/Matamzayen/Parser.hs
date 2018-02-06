module Matamzayen.Parser where

import Data.List.Split
import Matamzayen.BaseFuncs
import Data.Char
import Text.Regex

mainFuncNotation = '@'
funcSeparator = "."
funcNotation = "^"
commentNotation = "#"

removeWhitespaces :: String -> String
removeWhitespaces = filter (not . isSpace)

removeComments :: String -> String
removeComments str = subRegex (mkRegex reg) str ""
  where
    reg = commentNotation ++ ".+" ++ commentNotation

parse :: Floating a => String -> a -> a
parse program = getMainFunc funcsStrings
  where
    funcsStrings :: [String]
    funcsStrings = tail $ splitOn funcNotation ((removeComments . removeWhitespaces) program)

    funcsStringsWithoutMain :: [String]
    funcsStringsWithoutMain = filter (notElem mainFuncNotation) funcsStrings

    funcs :: Floating a => [a -> a]
    funcs = map createComposition funcsStringsWithoutMain

    funcFromString :: Floating a => String -> (a -> a)
    funcFromString str = getFuncByString str funcs

    createComposition :: Floating a => String -> (a -> a)
    createComposition = foldr1 (.) . map funcFromString . splitOn funcSeparator
    
    getMainFunc :: Floating a => [String] -> (a -> a)
    getMainFunc funcsStrings =
        let str = tail . head . filter (elem mainFuncNotation) $ funcsStrings
        in createComposition str

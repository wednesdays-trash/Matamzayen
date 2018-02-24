{-# LANGUAGE ScopedTypeVariables #-}

module Matamzayen.Parser where

import           Data.Char
import           Data.List.Split
import           Matamzayen.BaseFuncs
import           Text.Regex

mainFuncNotation = '@'
funcSeparator = "."
funcNotation = "^"
commentNotation = "#"

-- | A string containing a composition of function names. Ex: "double.inc.sqrt", "dec.negate" etc
type FuncString = String

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

removeComments :: String -> String
removeComments str = subRegex (mkRegex reg) str ""
  where reg = commentNotation ++ ".+" ++ commentNotation

parse :: String -> Double -> Double
parse program = getMainFunc funcsStrings
  where
    -- | An array containing each function declaration in the program as a string.
    --   For example, if the program is "^inc^double.sqrt", funcsString will contain ["inc", "double.sqrt"]
    --   The tail is there because since the first character of the program should be '^', the first
    --   string in the split array will be "".
    funcsStrings :: [FuncString]
    funcsStrings = tail $ splitOn funcNotation (removeComments program)

    funcsStringsWithoutMain :: [FuncString]
    funcsStringsWithoutMain = filter (notElem mainFuncNotation) funcsStrings

    -- | Creates an array of actual functions out of FuncsString-s.
    funcs :: [Double -> Double]
    funcs = map createComposition funcsStringsWithoutMain

    -- | Parses a string and returns its corresponding function.
    funcFromString :: String -> (Double -> Double)
    funcFromString str = Matamzayen.BaseFuncs.getFuncByString str funcs

    -- | Converts each function name in a FuncString into an actual function, then composes them all together.
    createComposition :: FuncString -> (Double -> Double)
    createComposition = foldr1 (.) . map (funcFromString . trim) . splitOn funcSeparator

    -- | Gets the main function out of an array of FuncString-s.
    getMainFunc :: [FuncString] -> (Double -> Double)
    getMainFunc funcsStrings =
        let str = tail . head . filter (elem mainFuncNotation) $ funcsStrings
        in createComposition str

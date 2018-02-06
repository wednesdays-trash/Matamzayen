module Matamzayen.Parser where

import Data.List.Split
import Matamzayen.BaseFuncs
import Data.Char
import Text.Regex

mainFuncNotation = '@'
funcSeparator = "."
funcNotation = "^"
commentNotation = "#"

-- ^ A string containing a composition of function names. Ex: "double.inc.sqrt", "dec.negate" etc
type FuncString = String

removeWhitespaces :: String -> String
removeWhitespaces = filter (not . isSpace)

removeComments :: String -> String
removeComments str = subRegex (mkRegex reg) str ""
  where
    reg = commentNotation ++ ".+" ++ commentNotation

parse :: Floating a => String -> a -> a
parse program = getMainFunc funcsStrings
  where
    -- | An array containing each function declaration in the program as a string.
    --   For example, if the program is "^inc^double.sqrt", funcsString will contain ["inc", "double.sqrt"]
    --   The tail is there because since the first character of the program should be '^', the first
    --   string in the split array will be "".
    funcsStrings :: [FuncString]
    funcsStrings = tail $ splitOn funcNotation ((removeComments . removeWhitespaces) program)

    funcsStringsWithoutMain :: [FuncString]
    funcsStringsWithoutMain = filter (notElem mainFuncNotation) funcsStrings

    -- | Creates an array of actual functions out of FuncsString-s.
    funcs :: Floating a => [a -> a]
    funcs = map createComposition funcsStringsWithoutMain

    -- | Parses a string and returns its corresponding function.
    funcFromString :: Floating a => String -> (a -> a)
    funcFromString str = Matamzayen.BaseFuncs.getFuncByString str funcs

    -- | Converts each function name in a FuncString into an actual function, then composes them all together.
    createComposition :: Floating a => FuncString -> (a -> a)
    createComposition = foldr1 (.) . map funcFromString . splitOn funcSeparator

    -- | Gets the main function out of an array of FuncString-s.
    getMainFunc :: Floating a => [FuncString] -> (a -> a)
    getMainFunc funcsStrings =
        let str = tail . head . filter (elem mainFuncNotation) $ funcsStrings
        in createComposition str

module Matamzayen.BaseFuncs where

import Control.Monad
import Data.List.Split
import qualified Safe

isCurried :: String -> Bool
isCurried = elem ' '

getTwoParamFuncByString :: String -> (Double -> Double)
getTwoParamFuncByString str = func (read param)
  where
    s = splitOn " " str
    funcName = head s
    param = s `Safe.at` 1
    func =
      case funcName of
        "add" -> (+)

getFuncByString :: String -> [Double -> Double] -> (Double -> Double)
getFuncByString str userFuncs =
  if isCurried str
    then getTwoParamFuncByString str
    else case str of
           "inc" -> (+ 1)
           "dec" -> subtract 1
           "double" -> (* 2)
           "half" -> (/ 2)
           "sqrt" -> sqrt
           "negate" -> negate
           "zero" -> const 0
           "id" -> id
           "square" -> join (*)
           "exp" -> exp
           "sin" -> sin
           "cos" -> cos
           "tan" -> tan
           n -> userFuncs `Safe.at` read n

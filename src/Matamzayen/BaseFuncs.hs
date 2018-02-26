module Matamzayen.BaseFuncs where

import Control.Monad
import Data.HashMap.Lazy
import Data.List.Split
import Data.Maybe
import qualified Safe

isCurried :: String -> Bool
isCurried = elem ' '

twoParamsFuncs :: HashMap String (Double -> Double -> Double)
twoParamsFuncs =
  fromList [("add", (+)), ("sub", (-)), ("div", (/)), ("mul", (*))]

getCurriedFuncByString :: String -> (Double -> Double)
getCurriedFuncByString funcString = func (read param)
  where
    s = splitOn " " funcString
    funcName = head s
    param = s `Safe.at` 1
    func =
      fromMaybe
        (\x y -> 5) -- temporary patch
        (Data.HashMap.Lazy.lookup funcName twoParamsFuncs)

funcs :: HashMap String (Double -> Double)
funcs =
  fromList
    [ ("inc", (+ 1))
    , ("dec", subtract 1)
    , ("double", (* 2))
    , ("half", (/ 2))
    , ("sqrt", sqrt)
    , ("negate", negate)
    , ("zero", const 0)
    , ("id", id)
    , ("square", join (*))
    , ("exp", exp)
    , ("sin", sin)
    , ("con", cos)
    , ("tan", tan)
    ]

getFuncByString :: String -> [Double -> Double] -> (Double -> Double)
getFuncByString funcName userFuncs =
  if isCurried funcName
    then getCurriedFuncByString funcName
    else fromMaybe
           (userFuncs `Safe.at` read funcName)
           (Data.HashMap.Lazy.lookup funcName funcs)

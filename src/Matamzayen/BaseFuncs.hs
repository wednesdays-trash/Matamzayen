module Matamzayen.BaseFuncs where

import Control.Monad

getFuncByString :: Floating a => String -> [a -> a] -> (a -> a)
getFuncByString str userFuncs =
  case str of
    "inc"    -> (+ 1)
    "dec"    -> subtract 1
    "double" -> (* 2)
    "half"   -> (/ 2)
    "sqrt"   -> sqrt
    "negate" -> negate
    "zero"   -> const 0
    "id"     -> id
    "square" -> join (*)
    "exp"    -> exp
    "sin"    -> sin
    "cos"    -> cos
    "tan"    -> tan
    n        -> userFuncs !! read n

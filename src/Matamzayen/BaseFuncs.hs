module Matamzayen.BaseFuncs where

getFuncByString :: Floating a => String -> [a -> a] -> (a -> a)
getFuncByString str userFuncs =
  case str of
    "inc" -> (+ 1)
    "dec" -> subtract 1
    "double" -> (* 2)
    "sqrt" -> sqrt
    n -> userFuncs !! read n

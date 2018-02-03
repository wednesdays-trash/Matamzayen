module Main where

import Matamzayen.Parser

fs :: [Double -> Double]
fs = parse "^double.double^sqrt.double.double"

main :: IO ()
main = print $ (fs !! 1) 2

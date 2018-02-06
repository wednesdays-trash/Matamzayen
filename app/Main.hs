module Main where

import Matamzayen.Parser
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case length args of
        1 -> do
            let input = read $ head args
            expr <- getLine
            print $ parseBlock expr input
        2 -> do
            let input = read $ args !! 1
            file <- readFile $ head args
            print $ parseBlock file input
        n -> do
            let inputs = map read $ tail args
            file <- readFile $ head args
            print $ parse file inputs

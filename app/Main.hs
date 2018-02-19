module Main where

import Matamzayen.Parser
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case length args of
        -- when only a source file is specified, we'd like to read stdin
        1 -> do
            let input = read $ head args
            expr <- getLine
            print $ parse expr input
        2 -> do
            let input = read $ args !! 1
            file <- readFile $ head args
            print $ parse file input

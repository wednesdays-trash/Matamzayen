module Main where

import Matamzayen.Parser
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  file <- readFile (head args)
  let input = read $ args !! 1
  print $ parse file input

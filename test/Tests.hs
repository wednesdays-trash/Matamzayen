module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Matamzayen.Parser

exampleProgram =
  "  ^double.double.inc\n^inc.2\n^double.double #neat comment#\n^@add 2.0.1  \n"

main = defaultMain (testGroup "Parser tests" tests)
  where
    tests = [removeCommentsTest, parseTest, trimTest]

newTest :: (Eq a, Show a) => String -> String -> a -> a -> TestTree
newTest testName description expectedResult expression =
  testCase testName assertion
  where
    assertion = assertEqual description expectedResult expression

removeCommentsTest =
  newTest
    "Testing comment removal"
    "Should return the string without comments (surrounded with '#')"
    "hey \n \n haha"
    (removeComments "hey \n#really cool comment# \n haha")

trimTest =
  newTest
    "Testing whitespace trimming"
    "Should remove trailing spaces from the edges of a string"
    "asddi388 #$#44"
    (trim "   asddi388 #$#44 \n \n \n")

parseTest =
  newTest
    "Testing actual program parsing and evaluation"
    "Should evaluate the given program and apply its main function on the input"
    58.0
    (parse exampleProgram input)
  where
    input = 3

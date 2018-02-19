module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Matamzayen.Parser

exampleProgram =
  "^double.double.inc\n^inc.2\n  ^double.double #neat comment#    ^@0.1"

main = defaultMain (testGroup "Parser tests" tests)
  where
    tests = [removeWhitespacesAndCommentsTest, parseTest]

removeWhitespacesAndCommentsTest =
  testCase "Testing removeWhitespaces and removeComments" assertion
  where
    expectedResult = "^double.double.inc^inc.2^double.double^@0.1"
    assertion =
      assertEqual
        "Should remove sapces, tabs, newlines and comments"
        expectedResult
        ((removeWhitespaces . removeComments) exampleProgram)

parseTest =
  testCase "Testing actual program parsing and evaluation" assertion
  where
    input = 3
    expectedResult = 56.0
    assertion =
      assertEqual
        "Should evaluate the given program and apply its main function on the input"
        expectedResult
        (parse exampleProgram input)

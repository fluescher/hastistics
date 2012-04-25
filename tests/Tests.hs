module Main where

import System.Exit (exitSuccess, exitFailure)
import System.IO (stdout)
import Test.HUnit

import Hastistics.Data


testListTableData   = ListTable ["One", "Other"] [[2,1], [1,2]]
testList            = testListTableData

simplestReport      = valueOf "One" $ sumOf "One" $ avgOf "Other" $
                      from testList

constraintReport    = valueOf "One" $ valueOf "Other" $
                      when (\r -> (fieldValueOf "One" r) == HSInt 1) $
                      from testList


emptyReport         = valueOf "One" $ sumOf "One" $ avgOf "One" $
                      when (\r -> (fieldValueOf "One" r)    == HSInt 1) $
                      when (\r -> (fieldValueOf "Other" r)  == HSInt 1) $
                      from testList

-- Test functions
testListTable       = TestCase $ assertEqual "Test list conversion" 
                                         [HSInt 2, HSInt 1]
                                         (valuesOf $ head (dataOf testListTableData))

testValueOfColumn   = TestCase $ assertEqual "Should get the raw value." [[HSInt 2, HSInt 3, HSDouble 1.5]] [valuesOf r | r <- dataOf (eval simplestReport )]


testConstraint      = TestCase $ assertEqual "Should have filtered out values" [[HSInt 1, HSInt 2]] [valuesOf r | r <- dataOf (eval constraintReport)]

testToStrict        = TestCase $ assertEqual "Should have filtered out all values" [[None, HSInt 0, None]] [valuesOf r | r <- dataOf (eval emptyReport)]


-- Register test functions here
listOfTests = [testListTable, testValueOfColumn, testConstraint, testToStrict]

main = do 
          (Counts cases tries errors failures) <- runTestTT $ TestList listOfTests
          if errors > 0 || failures > 0
               then exitFailure
               else exitSuccess

module Main where

import System.Exit (exitSuccess, exitFailure)
import System.IO (stdout)
import Test.HUnit

import Hastistics.Data


testListTableData   = ListTable ["One", "Other"] [[2, 1]]

-- Test functions
testListTable       = TestCase $ assertEqual "Test list conversion" 
                                         [HSInt 2, HSInt 1]
                                         (valuesOf $ head (dataOf testListTableData))

testFrom            = TestCase $ assertBool "No constraints expected." (length (constraints  (from testListTableData)) == 0)

-- Register test functions here
listOfTests = [ testListTable, testFrom ]

main = do 
          (Counts cases tries errors failures) <- runTestTT $ TestList listOfTests
          if errors > 0 || failures > 0
               then exitFailure
               else exitSuccess

module Main where

import System.Exit (exitSuccess, exitFailure)
import System.IO (stdout)
import Test.HUnit

import Hastistics.Data

-- Test function
testListTable   = TestCase $ assertEqual "Test list conversion" 
                                         [HSIntCol 2, HSIntCol 1]
                                         ( valuesOf $ head (dataOf (ListTable ["One", "Other"] [[2, 1]])))

-- Register test functions here
listOfTests = [ testListTable ]

main = do 
          (Counts cases tries errors failures) <- runTestTT $ TestList listOfTests
          if errors > 0 || failures > 0
               then exitFailure
               else exitSuccess

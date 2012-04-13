module Main where

import System.Exit (exitSuccess, exitFailure)
import System.IO (stdout)
import Test.HUnit

import Hastistics (t)

-- Test function
testTest = TestCase $ assertEqual "Test of the tests" 5 Hastistics.t


-- Register test functions here
listOfTests = [ testTest ]

main = do 
          (Counts cases tries errors failures) <- runTestTT $ TestList listOfTests
          if errors > 0 || failures > 0
               then exitFailure
               else exitSuccess

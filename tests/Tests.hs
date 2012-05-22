module Main where

import System.Exit (exitSuccess, exitFailure)
import System.IO (stdout)
import Test.HUnit

import Hastistics.Data
import Hastistics.Distributions


testListTableData   = ListTable ["One", "Other"] [[2,1], [1,2], [2,6]]
testList            = testListTableData

testBinominalTable 	= BinominalTable 4 0.2

simplestReport      = valueOf "One" $ sumOf "One" $ avgOf "Other" $
                      from testList

constraintReport    = valueOf "One" $ valueOf "Other" $
                      when (\r -> (fieldValueOf "One" r) == HSInt 1) $
                      from testList


emptyReport         = valueOf "One" $ sumOf "One" $ avgOf "One" $
                      when (\r -> (fieldValueOf "One" r)    == HSInt 1) $
                      when (\r -> (fieldValueOf "Other" r)  == HSInt 1) $
                      from testList


groupedReport       = valueOf "One" $ sumOf "Other" $
                      groupBy "One" $
                      from testList

probabilityReport	= sumOf "p" $
					  from testBinominalTable

listValueOf rep = [valuesOf r | r <- dataOf (eval rep)]

toHSInt :: HSValue -> HSValue
toHSInt (HSDouble d) = HSInt(floor d)
toHSInt _ = HSInt 0

-- Test functions
testListTable       = TestCase $ assertEqual "Test list conversion" 
                                         [HSInt 2, HSInt 1]
                                         (valuesOf $ head (dataOf testListTableData))

testValueOfColumn   = TestCase $ assertEqual "Should get the raw value." [[HSInt 2, HSInt 5, HSDouble 3]] (listValueOf simplestReport)


testConstraint      = TestCase $ assertEqual "Should have filtered out values" [[HSInt 1, HSInt 2]] (listValueOf constraintReport)

testToStrict        = TestCase $ assertEqual "Should have filtered out all values" [[None, HSInt 0, None]] (listValueOf emptyReport)


testGroupBy         = TestCase $ assertEqual "Should be grouped" [[HSInt 1, HSInt 2], [HSInt 2, HSInt 7]] (listValueOf groupedReport)

testProbability		= TestCase $ assertEqual "Should be equal to one" (HSInt 1) (toHSInt $ head $ head (listValueOf probabilityReport))

-- Register test functions here
listOfTests = [testListTable, testValueOfColumn, testConstraint, testToStrict, testGroupBy, testProbability]

main = do 
          (Counts cases tries errors failures) <- runTestTT $ TestList listOfTests
          if errors > 0 || failures > 0
               then exitFailure
               else exitSuccess

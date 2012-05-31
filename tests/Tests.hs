module Main where

import System.Exit (exitSuccess, exitFailure)
import System.IO (stdout)
import Test.HUnit

import Hastistics.Data.CSV
import Hastistics.Types
import Hastistics.Distributions
import Hastistics

testCsv             = "a,b,c,d,e\n1,this,is,a,test\n2,and,a,second,test\n3,asd,d,her,stuff\n"

testList            = ListTable ["One", "Other"] [[2,1], [1,2], [2,6]]
testListSimilar     = ListTable ["One", "Other"] [[2,1], [1,2], [2,1], [2,6], [2,6]]

testBinominalTable 	= BinominalTable 4 0.2

simplestReport      = valueOf "One" $ sumOf "One" $ avgOf "Other" $
                      from testList

countReport         = count $ from testList

maxReport			= maxOf "Other" $ from testList

constraintReport    = valueOf "One" $ valueOf "Other" $
                      when (\r -> (fieldValueOf "One" r) == HSInt 1) $
                      from testList

csvReport           = valueOf "a" $ count $
                      from (csvTable [toInt, toString, toString, toString, toString] testCsv)

emptyReport         = valueOf "One" $ sumOf "One" $ avgOf "One" $
                      when (\r -> (fieldValueOf "One" r)    == HSInt 1) $
                      when (\r -> (fieldValueOf "Other" r)  == HSInt 1) $
                      from testList

joinReport          = valueOf "b" $ valueOf "d" $
                      join (ListTable ["c", "d"] [[1,4],[2,8]]) "a" "c" $
                      groupBy "a" $
                      from (ListTable ["a", "b"] [[1,2],[2,4]])

joinToEmptyReport   = valueOf "b" $ valueOf "d" $
                      join (ListTable ["c", "d"] [[1,4],[2,8]]) "a" "c" $
                      groupBy "a" $
                      from (ListTable ["a", "b"] [[3,2],[4,4]])

multiJoinReport     = valueOf "a" $ valueOf "b" $ valueOf "c" $ valueOf "d" $
                      join (ListTable ["d"] [[1]]) "a" "d" $
                      join (ListTable ["b", "c"] [[1,56]]) "a" "b" $
                      from (ListTable ["a"] [[1]])

multiGroupReport    = valueOf "One" $ valueOf "Other" $
                      groupBy "One" $
                      groupBy "Other" $
                      when (\x -> (fieldValueOf "One" x) == HSInt 2) $
                      from testList

multiGroupReportS   = valueOf "One" $ valueOf "Other" $
                      groupBy "One" $
                      groupBy "Other" $
                      from testListSimilar

groupedReport       = valueOf "One" $ sumOf "Other" $
                      groupBy "One" $
                      from testList

probabilityReport	= sumOf "p" $
					  from testBinominalTable

listValueOf rep = [valuesOf r | r <- dataOf (select rep)]

toHSInt :: HSValue -> HSValue
toHSInt (HSDouble d) = HSInt(floor d)
toHSInt _ = HSInt 0

-- Test functions
testListTable       = TestCase $ assertEqual "Test list conversion" 
                                         [HSInt 2, HSInt 1]
                                         (valuesOf $ head (dataOf testList))

testValueOfColumn   = TestCase $ assertEqual "Should get the raw value." [[HSInt 2, HSInt 5, HSDouble 3]] (listValueOf simplestReport)

testCountField      = TestCase $ assertEqual "Should count the number of rows" [[HSInteger 3]] (listValueOf countReport)

testMaxField		= TestCase $ assertEqual "Should return the max value of a column" [[HSInt 6]] (listValueOf maxReport)

testConstraint      = TestCase $ assertEqual "Should have filtered out values" [[HSInt 1, HSInt 2]] (listValueOf constraintReport)

testToStrict        = TestCase $ assertEqual "Should have filtered out all values" [[None, HSInt 0, None]] (listValueOf emptyReport)


testGroupBy         = TestCase $ assertEqual "Should be grouped" [[HSInt 1, HSInt 2], [HSInt 2, HSInt 7]] (listValueOf groupedReport)

testProbability		= TestCase $ assertEqual "Should be equal to one" (HSInt 1) (toHSInt $ head $ head (listValueOf probabilityReport))

testJoin            = TestCase $ assertEqual "Should be joined with other table" [[HSInt 2, HSInt 4],[HSInt 4, HSInt 8]] (listValueOf joinReport)

testJoinToEmpty     = TestCase $ assertEqual "Should contain None values for each row" [[HSInt 2,None],[HSInt 4,None]] (listValueOf joinToEmptyReport)

testMultiJoin       = TestCase $ assertEqual "Should join multiple tables" [[HSInt 1, HSInt 1, HSInt 56, HSInt 1]] (listValueOf multiJoinReport)

testCSV             = TestCase $ assertEqual "Should parse CSV" [[HSInt 1, HSInteger 3]] (listValueOf csvReport)

testMultiGroup      = TestCase $ assertEqual "Should have two groups" [[HSInt 2, HSInt 1],[HSInt 2, HSInt 6]] (listValueOf multiGroupReport)

testMultiGroup2     = TestCase $ assertEqual "Shoult have three groups" [[HSInt 1, HSInt 2],[HSInt 2, HSInt 1],[HSInt 2, HSInt 6]] (listValueOf multiGroupReportS)

-- Register test functions here
listOfTests = [testListTable, testValueOfColumn, testConstraint, testToStrict, testGroupBy, testProbability, testJoin, testJoinToEmpty, testMultiJoin,
               testCountField, testCSV, testMultiGroup, testMultiGroup2, testMaxField]

main = do 
          (Counts cases tries errors failures) <- runTestTT $ TestList listOfTests
          if errors > 0 || failures > 0
               then exitFailure
               else exitSuccess

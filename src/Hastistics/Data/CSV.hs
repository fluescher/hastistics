module Hastistics.Data.CSV where
{-
import Hastistics.Data

data CSVTable = CSVFile [String] [HSCol] [HSRow]

instance HSTable CSVTable where
        colsOf (CSVFile _ columns _)= columns
        headersOf (CSVFile h _ _)   = [HSColHeader s | s <-h] 
        dataOf (CSVFile _ _ values) = values
-}

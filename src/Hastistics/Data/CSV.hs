module Hastistics.Data.CSV where
import Hastistics.Data

data CSVTable = CSVFile [HSCol] [HSRow]

instance HSTable CSVTable where
        colsOf (CSVFile columns _) = columns
        dataOf (CSVFile _ rows) = rows


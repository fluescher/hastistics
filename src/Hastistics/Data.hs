module Hastistics.Data where

{- |Supported columnt types. -}
data HSValue
   = HSString String
   | HSInt Int
   | None
   deriving(Eq, Show)

{- |Type which stands for a Column in a Table. -}
data HSCol      
   = HSCol   String 
   deriving(Eq, Show)

{- |Row in a table. Consists of a list of columns -}
data HSRow      
   = HSRow          [HSValue] 
   deriving(Eq, Show)

valuesOf :: HSRow -> [HSValue]
valuesOf (HSRow co) = co  


{- |Type class defining the interface to a Table implementation. Use this type class if you want to
define your own data sources for the Hastistics framework. -}
class HSTable t where
    colsOf  :: t -> [HSCol]
    dataOf  :: t -> [HSRow]

{- |Report structure storing all metainformation about a report. -}    
data (HSTable t) => HSReport t
   = HSReport {
                    source      :: t,
                    cols        :: [HSCol],
                    rows        :: [HSRow],
                    constraints :: [(HSRow -> Bool)]
              }

{- |Define the HSReport as a instance of a HSTable. This makes sure that results of a report can be 
used as input data in other reports. -}
instance HSTable t => HSTable (HSReport t) where
    colsOf = cols
    dataOf = rows

{- |Defines a report result table column wich contains the raw source value 
of the specified column. -}
valueOf  :: HSTable t =>  String -> HSReport t -> HSReport t
valueOf name repIn = repIn {cols= (HSCol name):(cols repIn)}

{- |Starting Point for every report run. Creates a new HSReport from 
a HSTable. -}
from        :: HSTable t => t -> HSReport t
from table  =  HSReport {source=table, cols=[], constraints=[], rows=[]}

{- |Used to filter input data of a HSReport. -}
when     :: HSTable t => (HSRow -> Bool) -> HSReport t -> HSReport t
when f report = report {constraints=f:(constraints report)}

data ListTable   = ListTable [String] [[Int]]
instance HSTable ListTable where
    colsOf (ListTable columns _)   = [ HSCol s | s <- columns ]
    dataOf (ListTable _ vals)  = [HSRow [HSInt f | f <- r ] | r <- vals]



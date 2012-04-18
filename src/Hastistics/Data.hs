module Hastistics.Data where

data HSCol      = HSHeaderCol String | HSIntCol Int | HSStringCol String | HSValueCol String
                  deriving(Eq, Show)
data HSRow      = HSRow [HSCol] 


class HSTable t where
    colsOf  :: t -> [HSCol]
    dataOf  :: t -> [HSRow]

    
data (HSTable t) => HSReport t
     = HSReport {
                    source      :: t,
                    cols        :: [HSCol],
                    rows        :: [HSRow],
                    constraints :: [(HSRow -> Bool)]
                }

instance HSTable t => HSTable (HSReport t) where
    colsOf = cols
    dataOf = rows

valueOf  :: HSTable t =>  String -> HSReport t -> HSReport t
valueOf name repIn = repIn {cols= (HSValueCol name):(cols repIn)}

valuesOf :: HSRow -> [HSCol]
valuesOf (HSRow co) = co  

from     :: HSTable t => t -> HSReport t
from table  =  HSReport {source=table, cols=[], constraints=[], rows=[]}


data ListTable   = ListTable [String] [[Int]]
instance HSTable ListTable where
    colsOf (ListTable columns _)   = [ HSHeaderCol s | s <- columns ]
    dataOf (ListTable _ vals)  = [HSRow [HSIntCol f | f <- r ] | r <- vals]

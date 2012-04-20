module Hastistics.Data where

data HSValue
   = HSString String
   | HSInt Int
   | HSDouble Double
   | None
   deriving(Eq, Show)

data HSField
   = HSStaticField      HSValue
   | HSCalcField  HSValue (HSRow -> HSField -> HSField)

{- |Get the value out of a HSField -}
val :: HSField -> HSValue
val (HSStaticField v)   = v
val (HSCalcField v _)   = v


{- |Row in a table. Consists of a list of columns -}
data HSRow      
   = HSValueRow          [String] [HSField] 

{- |Get the values out of a HSRow. -}
valuesOf :: HSRow -> [HSValue]
valuesOf (HSValueRow _ vs)    = [val v | v <- vs]


fieldValueOf :: String -> HSRow -> HSValue
fieldValueOf _ (HSValueRow _ [])            = None
fieldValueOf _ (HSValueRow [] _)            = None
fieldValueOf col (HSValueRow (h:hs) (v:vs)) | h == col   = val v
                                            | otherwise  = fieldValueOf col (HSValueRow hs vs)


{- |Type class defining the interface to a Table implementation. Use this type class if you want to
define your own data sources for the Hastistics framework. -}
class HSTable t where
    headersOf   :: t -> [String]
    colsOf      :: t -> [HSField]
    dataOf      :: t -> [HSRow]

{- |Report structure storing all metainformation about a report. -}    
data (HSTable t) => HSReport t
   = HSReport {
                    source      :: t,
                    headers     :: [String],
                    cols        :: [HSField],
                    rows        :: [HSRow],
                    constraints :: [(HSRow -> Bool)]
              }

{- |Define the HSReport as a instance of a HSTable. This makes sure that results of a report can be 
used as input data in other reports. -}
instance HSTable t => HSTable (HSReport t) where
    headersOf = headers
    colsOf    = cols
    dataOf    = rows


valueOfUpdater :: String -> HSRow -> HSField -> HSValue
valueOfUpdater _ _ (HSStaticField v)            = v
valueOfUpdater col row (HSCalcField _  _)  = fieldValueOf col row


{- |Starting Point for every report run. Creates a new HSReport from 
a HSTable. -}
from        :: HSTable t => t -> HSReport t
from table  =  HSReport {source=table, cols=[], constraints=[], rows=[], headers=[]}

{- |Used to filter input data of a HSReport. -}
when     :: HSTable t => (HSRow -> Bool) -> HSReport t -> HSReport t
when f report = report {constraints=f:(constraints report)}

data ListTable   = ListTable [String] [[Int]]
instance HSTable ListTable where
    headersOf (ListTable hs _) = hs 
    colsOf (ListTable _  _)    = []
    dataOf (ListTable hs vals)  = [HSValueRow hs [HSStaticField (HSInt f) | f <- r ] | r <- vals]



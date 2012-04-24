{-# LANGUAGE ExistentialQuantification #-}

module Hastistics.Data where

data HSValue
   = HSString String
   | HSInt Int
   | HSDouble Double
   | None
   deriving(Eq, Show)

class HSField f where
    val         :: f -> HSValue
    update      :: f -> HSRow -> f
    update fi r =  fi

data HSStaticField  = HSStaticField HSValue
instance HSField HSStaticField where
    val (HSStaticField v)   = v
    update f _              = f

data HSFieldHolder = forall a. HSField a => HSFieldHolder a

pack    :: HSField a => a -> HSFieldHolder
pack    = HSFieldHolder



{- |Row in a table. Consists of a list of columns -}
data HSRow      
   = HSValueRow  [String] [HSFieldHolder] 

{- |Get the values out of a HSRow. -}
valuesOf :: HSRow -> [HSValue]
valuesOf (HSValueRow _ vs)    = [val v | (HSFieldHolder v) <- vs]


fieldValueOf :: String -> HSRow -> HSValue
fieldValueOf _ (HSValueRow _ [])                            = None
fieldValueOf _ (HSValueRow [] _)                            = None
fieldValueOf col (HSValueRow (h:hs) ((HSFieldHolder v):vs)) | h == col   = val v
                                                            | otherwise  = fieldValueOf col (HSValueRow hs vs)


{- |Type class defining the interface to a Table implementation. Use this type class if you want to
define your own data sources for the Hastistics framework. -}
class HSTable t where
    headersOf   :: t -> [String]
    colsOf      :: t -> [HSFieldHolder]
    dataOf      :: t -> [HSRow]


{- |Report structure storing all metainformation about a report. -}    
data (HSTable t) => HSReport t
   = HSReport {
                    source      :: t,
                    headers     :: [String],
                    cols        :: [HSFieldHolder],
                    rows        :: [HSRow],
                    constraints :: [(HSRow -> Bool)]
              }

{- |Define the HSReport as a instance of a HSTable. This makes sure that results of a report can be 
used as input data in other reports. -}
instance HSTable t => HSTable (HSReport t) where
    headersOf = headers
    colsOf    = cols
    dataOf    = rows


data HSValueOfField = HSValueOfField String HSValue

instance HSField HSValueOfField where
    val     (HSValueOfField _ v)    = v 
    update  (HSValueOfField h _) r  = HSValueOfField h (fieldValueOf h r)

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
    dataOf (ListTable hs vals)  = [HSValueRow hs [pack (HSStaticField (HSInt f)) | f <- r ] | r <- vals]



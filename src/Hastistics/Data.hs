{-# LANGUAGE ExistentialQuantification #-}

module Hastistics.Data where

data HSValue
   = HSString String
   | HSInt Int
   | HSDouble Double
   | None
   deriving(Eq, Show)


(+)     :: HSValue -> HSValue -> HSValue
(+) (HSString sa)    (HSString sb) = HSString (sa Prelude.++ sb)
(+) (HSInt ia)       (HSInt ib)    = HSInt (ia Prelude.+ ib)
(+) (HSDouble da)    (HSDouble db) = HSDouble (da Prelude.+ db)
(+) _               _              = None

(/)     :: HSValue -> HSValue -> HSValue
(/) (HSInt ia)      (HSInt ib)     = HSInt (div ia ib)
(/) (HSDouble da)   (HSDouble db)  = HSDouble (da Prelude./ db)
(/) _               _              = None


class HSField f where
    val         :: f -> HSValue
    update      :: f -> HSRow -> f
    update fi _ =  fi

data HSStaticField  = HSStaticField HSValue
instance HSField HSStaticField where
    val (HSStaticField v)   = v
    update f _              = f

data HSValueOfField = HSValueOfField String HSValue

instance HSField HSValueOfField where
    val     (HSValueOfField _ v)    = v 
    update  (HSValueOfField h _) r  = HSValueOfField h (fieldValueOf h r)


data HSAvgField = HSAvgField String HSValue Int

instance HSField HSAvgField where
    val     (HSAvgField _ su cnt)   = su Hastistics.Data./ HSDouble (fromIntegral cnt)
    update  (HSAvgField h su cnt) r = HSAvgField h (su Hastistics.Data.+ (fieldValueOf h r)) (1 Prelude.+ cnt)


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

{- |Adds a simple result column to the report. This column contains the
unmodified value of the source column. -}
valueOf     :: HSTable t => String -> HSReport t -> HSReport t
valueOf h r = r{cols= (pack (HSValueOfField h None)):(cols r)}

{- |Starting Point for every report run. Creates a new HSReport from 
a HSTable. -}
from        :: HSTable t => t -> HSReport t
from table  =  HSReport {source=table, cols=[], constraints=[], rows=[], headers=[]}

{- |Used to filter input data of a HSReport. -}
when        :: HSTable t => (HSRow -> Bool) -> HSReport t -> HSReport t
when f report = report {constraints=f:(constraints report)}

data ListTable   = ListTable [String] [[Int]]
instance HSTable ListTable where
    headersOf (ListTable hs _) = hs 
    colsOf (ListTable _  _)    = []
    dataOf (ListTable hs vals)  = [HSValueRow hs [pack (HSStaticField (HSInt f)) | f <- r ] | r <- vals]






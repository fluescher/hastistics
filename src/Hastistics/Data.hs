{-# LANGUAGE ExistentialQuantification #-}

module Hastistics.Data
       (HSValue(HSString, HSInt, HSDouble, None),
        HSTable, ListTable(ListTable), HSReport,
        from, groupBy, when, avgOf, valueOf, sumOf,
        dataOf, valuesOf, fieldValueOf,
        eval) where

import qualified Data.Map as Map

data HSValue
   = HSString String
   | HSInt Int
   | HSDouble Double
   | None
   deriving(Eq, Ord, Show)

type Key = String

(+)     :: HSValue -> HSValue -> HSValue
(+) (HSString sa)    (HSString sb) = HSString (sa Prelude.++ sb)
(+) (HSInt ia)       (HSInt ib)    = HSInt (ia Prelude.+ ib)
(+) (HSDouble da)    (HSDouble db) = HSDouble (da Prelude.+ db)
(+) (HSDouble da)    (HSInt ib)    = HSDouble (da Prelude.+ (fromIntegral ib))
(+) (HSInt ia)       (HSDouble db) = HSDouble ((fromIntegral ia) Prelude.+ db)
(+) _               _              = None

(/)     :: HSValue -> HSValue -> HSValue
(/) _               (HSInt 0)      = None
(/) _               (HSDouble 0)   = None
(/) (HSInt ia)      (HSInt ib)     = HSInt (div ia ib)
(/) (HSDouble da)   (HSDouble db)  = HSDouble (da Prelude./ db)
(/) (HSDouble da)   (HSInt ib)     = HSDouble (da Prelude./ (fromIntegral ib))
(/) (HSInt ia)      (HSDouble db)  = HSDouble ((fromIntegral ia) Prelude./ db)
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
    val     (HSValueOfField _ v)       = v 
    update  (HSValueOfField h None) r  = HSValueOfField h (fieldValueOf h r)
    update  f _                        = f


data HSAvgField = HSAvgField String HSValue Int
instance HSField HSAvgField where
    val     (HSAvgField _ su cnt)   = su Hastistics.Data./ HSDouble (fromIntegral cnt)
    update  (HSAvgField h su cnt) r = HSAvgField h sm newCnt
                                    where sm     = (Hastistics.Data.+) su (fieldValueOf h r)
                                          newCnt = (Prelude.+) 1 cnt

data HSSumField = HSSumField String HSValue
instance HSField HSSumField where
    val     (HSSumField _ v)   = v
    update  (HSSumField h v) r = HSSumField h ((Hastistics.Data.+) v (fieldValueOf h r))


data HSFieldHolder = forall a. HSField a => HSFieldHolder a

pack    :: HSField a => a -> HSFieldHolder
pack    = HSFieldHolder



{- |Row in a table. Consists of a list of columns -}
data HSRow      
   = HSValueRow  [Key] [HSFieldHolder] 

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
    headersOf   :: t -> [Key]
    dataOf      :: t -> [HSRow]
    lookup	:: String -> Key -> t -> [HSRow]

data ListTable   = ListTable [Key] [[Int]]
instance HSTable ListTable where
    headersOf (ListTable hs _) = hs 
    dataOf (ListTable hs vals)  = [HSValueRow hs [pack (HSStaticField (HSInt f)) | f <- r ] | r <- vals]
    lookup _ _ _	       = []

data HSResult   = HSEmptyResult
                | HSSingleResult HSRow
                | HSGroupedResult Key HSRow (Map.Map HSValue HSRow)

reportResult   :: HSReport -> [HSRow]
reportResult r = resultRows (rows r)

resultRows     :: HSResult -> [HSRow]
resultRows HSEmptyResult            = []
resultRows (HSSingleResult r)       = [r]
resultRows (HSGroupedResult _ _ rs) = [row | (_, row) <- Map.toList rs]


data HSTableHolder = forall a. HSTable a => HSTableHolder a

{- |Report structure storing all metainformation about a report. -}    
data HSReport
   = HSReport {
                    source      :: HSTableHolder,
                    headers     :: [Key],
                    cols        :: [HSFieldHolder],
                    rows        :: HSResult,
                    constraints :: [(HSRow -> Bool)],
                    groupKey    :: Maybe Key
              }

addCalcCol      :: HSField f => HSReport -> f -> HSReport
addCalcCol r f  = r{cols = (pack f):(cols r)} 

addConstraint   :: HSReport -> (HSRow -> Bool) -> HSReport
addConstraint r f = r{constraints=f:(constraints r)}

shouldInclude   :: HSReport -> HSRow -> Bool
shouldInclude report row = and [cond row | cond <- constraints report]

{- |Define the HSReport as a instance of a HSTable. This makes sure that results of a report can be 
used as input data in other reports. -}
instance HSTable HSReport where
    headersOf = headers
    dataOf    = reportResult
    lookup _ _ _    = []

{- |Adds a simple result column to the report. This column contains the
unmodified value of the source column. -}
valueOf     :: String -> HSReport -> HSReport 
valueOf h r = addCalcCol r field
              where field = HSValueOfField h None

{- |Adds a result column to the report. This column sums the values 
of the provided source column. -}
sumOf       :: String -> HSReport -> HSReport
sumOf   h r = addCalcCol r field
              where field = HSSumField h (HSInt 0)

{- |Adds a result column to the report. This column calculates the average
value of the value. -}
avgOf       :: String -> HSReport -> HSReport
avgOf   h r = addCalcCol r field
              where field = HSAvgField h (HSDouble 0) 0

groupBy     :: Key -> HSReport -> HSReport
groupBy k r = r {groupKey=Just k}

{- |Starting Point for every report run. Creates a new HSReport from 
a HSTable. -}
from        :: HSTable t => t -> HSReport
from table  =  HSReport {source=HSTableHolder table, cols=[], constraints=[], rows=HSEmptyResult, headers=[], groupKey=Nothing}

{- |Used to filter input data of a HSReport. -}
when        :: (HSRow -> Bool) -> HSReport -> HSReport
when f report = addConstraint report f

eval        :: HSReport -> HSReport
eval report | isGrouped report  = evalReport (report{rows= HSGroupedResult (groupKeyFor (groupKey report)) (HSValueRow (headers report)(cols report)) Map.empty})
            | otherwise         = evalReport (report{rows= HSSingleResult (HSValueRow (headers report) (cols report))})


groupKeyFor :: (Maybe Key) -> Key
groupKeyFor (Just k) = k
groupKeyFor _ = error "No group by key defined"

isGrouped  :: HSReport -> Bool
isGrouped  report = not ((groupKey report) == Nothing)


updateRow :: HSRow -> HSRow -> HSRow
updateRow (HSValueRow hs fs) row = HSValueRow hs [pack (update c row) | (HSFieldHolder c) <- fs]

updateResults :: HSResult -> [HSRow] -> HSResult
updateResults res (r:rs)  = updateResults (updateResult res r) rs
updateResults res []      = res

updateResult :: HSResult -> HSRow -> HSResult
updateResult (HSSingleResult row) dat           = HSSingleResult (updateRow row dat)
updateResult (HSGroupedResult k proto rs) dat   = HSGroupedResult k proto (Map.alter f (fieldValueOf k dat) rs)
                                                  where f = updateOrCreate proto dat
updateResult res _ = res

updateOrCreate :: HSRow -> HSRow -> (Maybe HSRow) -> (Maybe HSRow)
updateOrCreate _   dat (Just row) = Just (updateRow row dat)
updateOrCreate row dat Nothing    = Just (updateRow row dat)


evalReport :: HSReport -> HSReport
evalReport report = report {rows= updateResults (rows report) dat}
                  where dat                       = filter predicate (toDat (source report))
                        toDat (HSTableHolder tab) = dataOf tab
                        predicate                 = shouldInclude report




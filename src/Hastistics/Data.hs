{-# LANGUAGE ExistentialQuantification #-}

module Hastistics.Data where

import Hastistics.Types
import Hastistics.Fields
import qualified Data.Map as Map

data ListTable   = ListTable [Key] [[Int]]

instance HSTable ListTable where
    headersOf (ListTable hs _) = hs 
    dataOf (ListTable hs vals)  = [HSValueRow hs [pack (HSStaticField (HSInt f)) | f <- r ] | r <- vals]

instance Show ListTable where
    show = showTable

data HSResult   = HSEmptyResult
                | HSSingleResult HSRow
                | HSGroupedResult Key HSRow (Map.Map HSValue HSRow)

reportResult   :: HSReport -> [HSRow]
reportResult r = resultRows (rows r)

resultRows     :: HSResult -> [HSRow]
resultRows HSEmptyResult            = []
resultRows (HSSingleResult r)       = [r]
resultRows (HSGroupedResult _ _ rs) = [row | (_, row) <- Map.toList rs]

toRow        :: [Key] -> [HSValue] -> HSRow
toRow ks vs  = HSValueRow ks [pack (HSStaticField v) | v <- vs]


data HSTableHolder = forall a. HSTable a => HSTableHolder a

{- |Report structure storing all metainformation about a report. -}    
data HSReport
   = HSReport {
                    source      :: HSTableHolder,
                    headers     :: [Key],
                    cols        :: [HSFieldHolder],
                    rows        :: HSResult,
                    constraints :: [(HSRow -> Bool)],
                    groupKey    :: Maybe Key,
                    joinKeys    :: [JoinInfo]
              }

addCalcCol      :: HSField f => HSReport -> f -> HSReport
addCalcCol r f  = r{cols = (pack f):(cols r), headers = (meta f) : (headers r)} 

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

instance Show HSReport where
    show = showTable


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

{- |Counts the number of input rows. -}
count       :: HSReport -> HSReport
count   r   = addCalcCol r field
              where field = HSCountField (HSInteger 0)


{- |Adds a result column to the report. This column calculates the average
value of the value. -}
avgOf       :: String -> HSReport -> HSReport
avgOf   h r = addCalcCol r field
              where field = HSAvgField h (HSDouble 0) 0

data JoinInfo = JoinInfo Key Key HSTableHolder

join        :: HSTable t => t -> Key -> Key -> HSReport -> HSReport
join t a b r = r {joinKeys=(JoinInfo a b (HSTableHolder t)):joinKeys r}

groupBy     :: Key -> HSReport -> HSReport
groupBy k r = r {groupKey=Just k}

{- |Starting Point for every report run. Creates a new HSReport from 
a HSTable. -}
from        :: HSTable t => t -> HSReport
from table  =  HSReport {source=HSTableHolder table, cols=[], constraints=[], rows=HSEmptyResult, headers=[], groupKey=Nothing, joinKeys=[]}

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

combine             :: HSRow -> HSRow -> HSRow
combine (HSValueRow onehs onefs) (HSValueRow otherhs otherfs)   = HSValueRow (onehs ++ otherhs) (onefs ++ otherfs)

joinedData              :: JoinInfo -> HSRow -> [HSRow]
joinedData (JoinInfo leftKey rightKey (HSTableHolder tab)) row    = datOrPlaceHolder (Hastistics.Types.lookup rightKey joinVal tab)
                                                                  where datOrPlaceHolder [] = [toRow (headersOf tab) (take (length (headersOf tab)) (repeat None))]
                                                                        datOrPlaceHolder xs = xs
                                                                        joinVal             = fieldValueOf leftKey row

sourceData      :: HSReport -> [HSRow]
sourceData r    = toDat (joinKeys r) (source r)
                where toDat []       (HSTableHolder tab) = dataOf tab
                      toDat js (HSTableHolder tab)       = [compose left js | left <- dataOf tab]
                      compose left js                    = foldl (combine) left [(head (joinedData jin left)) | jin <- js]


evalReport :: HSReport -> HSReport
evalReport report = report {rows= updateResults (rows report) dat}
                  where dat                       = filter predicate (sourceData report)
                        predicate                 = shouldInclude report


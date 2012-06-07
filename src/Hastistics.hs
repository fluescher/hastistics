{-# LANGUAGE ExistentialQuantification #-}
module Hastistics where

import Hastistics.Types
import Hastistics.Fields
import qualified Data.Map as Map

{- | Simple implementation of a in memory table. Values are mapped to HSValues  -}
data ListTable   = ListTable [Key] [[Int]]

instance HSTable ListTable where
    headersOf (ListTable hs _) = hs 
    dataOf (ListTable hs vals)  = [HSValueRow hs [pack (HSStaticField (HSInt f)) | f <- r ] | r <- vals]

instance Show ListTable where
    show = showTable

{- | Result Type of a report. -}
data HSResult   = HSEmptyResult
                | HSSingleResult HSRow
                | HSByRowResult [HSRow]
                | HSGroupedResult (Map.Map HSValue HSResult)

{- | Returns the result data rows of an evaluated report. -}
reportResult   :: HSReport -> [HSRow]
reportResult r = resultRows (result r)

{- | Returns the result data rows of an report result. -}
resultRows     :: HSResult -> [HSRow]
resultRows HSEmptyResult              = []
resultRows (HSSingleResult r)         = [r]
resultRows (HSGroupedResult rs)       = fromMultiList $ groupedToList [res | (_, res) <- Map.toList rs]
resultRows (HSByRowResult rs)         = rs


{- | Flattens a list of lists to a list. -}
fromMultiList         :: [[a]] -> [a]
fromMultiList (x:xs)  = [v | v <- x] ++ fromMultiList xs
fromMultiList []      = []

{- | Flattens a grouped result tree structure to a HSRow list. -}
groupedToList           :: [HSResult] -> [[HSRow]]
groupedToList (r:rs)    = resultRows r : groupedToList rs
groupedToList []        = []

{- | Creates a new HSRow.  -}
toRow        :: [Key] -> [HSValue] -> HSRow
toRow ks vs  = HSValueRow ks [pack (HSStaticField v) | v <- vs]

type HSJoiner           = HSRow -> HSRow
type HSConstraint       = HSRow -> Bool
type HSResultPrototype  = HSRow
type HSResultUpdater    = HSResult -> HSRow -> HSResultPrototype-> HSResult
data HSTableHolder      = forall a. HSTable a => HSTableHolder a

{- |Report structure storing all metainformation about a report. -}    
data HSReport
   = HSReport {
                    source      :: HSTableHolder,
                    headers     :: [Key],
                    sheaders    :: [Key],
                    proto       :: HSRow,
                    result      :: HSResult,
                    constraints :: HSConstraint,
                    updater     :: HSResultUpdater,
                    joiner      :: HSJoiner
              }

{-| Add a calculated field to the definition of the report. -}
addCalcCol      :: HSField f => HSReport -> f -> HSReport
addCalcCol r f  = r{proto=HSValueRow ((meta f):(rowHeaders (proto r))) ((pack f):(rowFields (proto r))), headers= meta f : headers r} 

{-| Add a constraint to the constraints of this report. -}
addConstraint   :: HSReport -> HSConstraint -> HSReport
addConstraint r f = r{constraints=(\row -> (f row) && (constraints r) row)}


{- |Define the HSReport as a instance of a HSTable. This makes sure that results of a report can be 
used as input data in other reports. -}
instance HSTable HSReport where
    headersOf       = headers
    dataOf          = reportResult

{- | Because a HSReport is a HSTable, the showTable function is used -}
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


{- | Adds a result column to the report. This column calculates the average
value of the value. -}
avgOf       :: String -> HSReport -> HSReport
avgOf   h r = addCalcCol r field
              where field = HSAvgField h (HSDouble 0) 0

{- | Calculates the minimum value of a column. -}
minOf       :: String -> HSReport -> HSReport
minOf   h r = addCalcCol r field
              where field = HSMinField h (HSDouble infinity)

{- | Calculates the max value of a column. -}
maxOf       :: String -> HSReport -> HSReport
maxOf   h r = addCalcCol r field
              where field = HSMaxField h (HSDouble negativInfinity)

{- | Returns the Result by Row -}
byrow       :: HSReport -> HSReport
byrow    r  = r {updater= byrowUpdater}

infinity :: Double
infinity = 1 Prelude./ 0

negativInfinity :: Double
negativInfinity = -infinity

{- | Calculates the median of a row. -}
medianOf       :: String -> HSReport -> HSReport
medianOf   h r = addCalcCol r field
              where field = HSMedianField h []
{- | Add a custom calculation. -}
cust        :: String -> (HSValue -> HSRow -> HSValue) -> HSReport -> HSReport
cust s f r  = addCalcCol r field
              where field = HSCustField s None (f) 

{- | Updates the existing join function with a new join source. -}
join        :: HSTable t => t -> Key -> Key -> HSReport -> HSReport
join t a b r = r {joiner=(joinRow t a b) . (joiner r), sheaders=(sheaders r) ++ (headersOf t)}

{- | Updates the group updater with a new groupKey. -}
groupBy     :: Key -> HSReport -> HSReport
groupBy k r = r {updater= groupUpdater k (updater r)}

{- |Starting Point for every report. Creates a new HSReport from 
a HSTable. -}
from        :: HSTable t => t -> HSReport
from table  =  HSReport {
                         source=HSTableHolder table, 
                         proto=HSValueRow [] [], 
                         constraints=(\_ -> True), 
                         result=HSEmptyResult, 
                         headers=[], 
                         sheaders=headersOf table, 
                         updater=singleUpdater, 
                         joiner=(\a -> a)
                         }

{- | Used to filter input data of a HSReport. -}
when        :: (HSRow -> Bool) -> HSReport -> HSReport
when f report = addConstraint report f

{- | Updates the result row by row -}
byrowUpdater                            :: HSResultUpdater
byrowUpdater (HSByRowResult rs) row p    = HSByRowResult ((updateRow p row):rs)
byrowUpdater _ row p      		 = HSByRowResult [updateRow p row]

{- | Function which updates a single result. -}
singleUpdater                           :: HSResultUpdater
singleUpdater (HSSingleResult res) row _ = HSSingleResult (updateRow res row)
singleUpdater HSEmptyResult row p        = HSSingleResult (updateRow  p row)
singleUpdater r _ _                      = r

{- | Function which updates a grouped result. -}
groupUpdater                             :: Key -> HSResultUpdater -> HSResultUpdater
groupUpdater k u (HSGroupedResult m) row prot    = updateGroupedResult k u prot row m
groupUpdater k u _  row prot                     = updateGroupedResult k u prot row Map.empty

{- | Updates a grouped result. -}
updateGroupedResult :: Key -> HSResultUpdater -> HSResultPrototype -> HSRow -> Map.Map HSValue HSResult -> HSResult
updateGroupedResult k u prot row m = HSGroupedResult (Map.alter f (fieldValueOf k row) m)
                                     where f = updateOrInsert u row prot

{- | Update a given result or create a new one if the result was empty. -}
updateOrInsert  :: HSResultUpdater -> HSRow -> HSResultPrototype -> Maybe HSResult -> Maybe HSResult
updateOrInsert  u row prot (Just res)  = Just (u res row prot)
updateOrInsert  u row prot Nothing     = Just (u HSEmptyResult row prot)

{- | Returns the matches on a given table with equalvalues in the columns specified by the two String arguments and the column
     in the HSRow. -}
joinData :: HSTable t => t -> String -> String -> HSRow -> [HSRow]
joinData tab leftKey rightKey row =  datOrPlaceHolder (Hastistics.Types.lookup rightKey joinVal tab)
                                     where datOrPlaceHolder [] = [toRow (headersOf tab) (take (length (headersOf tab)) (repeat None))]
                                           datOrPlaceHolder xs = xs
                                           joinVal             = fieldValueOf leftKey row

{- | Combines the two rows by appending the second argument on the left side of the first. -}
combine             :: HSRow -> HSRow -> HSRow
combine (HSValueRow onehs onefs) (HSValueRow otherhs otherfs)  = HSValueRow (onehs ++ otherhs) (onefs ++ otherfs)

{- | Joins the row with the data of the given table using the two columns identified by the String arguments. -}
joinRow                     :: HSTable t => t -> String -> String -> HSRow -> HSRow
joinRow tab left right  row =  combine row (head $ joinData tab left right row)

{- | Select the data of the report. This evaluates the report accessing all the source tables. -}
select      :: HSReport -> HSReport
select      = evalReport

{- | updates all HSFields int the provided HSRow with the data in the second argument. -}
updateRow :: HSRow -> HSRow -> HSRow
updateRow (HSValueRow hs fs) row = HSValueRow hs [pack (update c row) | (HSFieldHolder c) <- fs]

{- | update the HSResult with the data in the second argument using the REsultPrototype -}
updateResults :: HSResult -> [HSRow] -> HSResultPrototype -> HSResultUpdater -> HSResult
updateResults res (r:rs)  p u   = updateResults (u res r p) rs p u
updateResults res []      _ _  = res

{- | Returns the joined data from all the source tables.  -}
sourceData      :: HSReport -> [HSRow]
sourceData r    = map (joinIt) (sourceDat (source r))
                  where sourceDat (HSTableHolder t)     = dataOf t
                        joinIt                          = setHeader (sheaders r) . joiner r
                        setHeader hs (HSValueRow _ vs)  = HSValueRow hs vs

{- | Evaluates the HSReport definition filling up the result value on the provided HSReport. -}
evalReport :: HSReport -> HSReport
evalReport report = report {result= updateResults (HSSingleResult prot) dat prot updat}
                  where updat       = updater report
                        prot        = proto report
                        dat         = filter predicate (sourceData report)
                        predicate   = constraints report

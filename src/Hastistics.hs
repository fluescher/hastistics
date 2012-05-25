{-# LANGUAGE ExistentialQuantification #-}
module Hastistics where

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
                | HSGroupedResult Key HSRow (Map.Map HSValue HSResult) HSResultUpdater

reportResult   :: HSReport -> [HSRow]
reportResult r = resultRows (result r)

resultRows     :: HSResult -> [HSRow]
resultRows HSEmptyResult              = []
resultRows (HSSingleResult r)         = [r]
resultRows (HSGroupedResult _ _ rs _) = fromMultiList $ groupedToList [res | (_, res) <- Map.toList rs]

fromMultiList :: [[a]] -> [a]
fromMultiList (x:xs)  = [v | v <- x] ++ fromMultiList xs
fromMultiList []      = []

groupedToList           :: [HSResult] -> [[HSRow]]
groupedToList (r:rs)    = resultRows r : groupedToList rs
groupedToList []        = []

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

addCalcCol      :: HSField f => HSReport -> f -> HSReport
addCalcCol r f  = r{proto=HSValueRow ((meta f):(rowHeaders (proto r))) ((pack f):(rowFields (proto r)))} 

addConstraint   :: HSReport -> HSConstraint -> HSReport
addConstraint r f = r{constraints=(\row -> (f row) && (constraints r) row)}


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

minOf       :: String -> HSReport -> HSReport
minOf   h r = addCalcCol r field
              where field = HSMinField h (HSDouble infinity)

infinity :: Double
infinity = 1 Prelude./ 0

join        :: HSTable t => t -> Key -> Key -> HSReport -> HSReport
join t a b r = r {joiner=(joinRow t a b) . (joiner r), sheaders=(sheaders r) ++ (headersOf t)}

groupBy     :: Key -> HSReport -> HSReport
groupBy k r = r {updater= groupUpdater k (updater r)}

{- |Starting Point for every report run. Creates a new HSReport from 
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

{- |Used to filter input data of a HSReport. -}
when        :: (HSRow -> Bool) -> HSReport -> HSReport
when f report = addConstraint report f

singleUpdater                            :: HSResultUpdater
singleUpdater (HSSingleResult res) row _ = HSSingleResult (updateRow res row)
singleUpdater r _ _                      = r

groupUpdater                             :: Key -> HSResultUpdater -> HSResultUpdater
groupUpdater k u (HSSingleResult _) row prot        = makeGroupedResult k u prot row Map.empty
groupUpdater k u HSEmptyResult      row prot        = makeGroupedResult k u prot row Map.empty
groupUpdater k u (HSGroupedResult _ _ m _) row prot = makeGroupedResult k u prot row m

makeGroupedResult :: Key -> HSResultUpdater -> HSResultPrototype -> HSRow -> Map.Map HSValue HSResult -> HSResult
makeGroupedResult k u prot row m = HSGroupedResult k prot (Map.alter f (fieldValueOf k row) m) u
                                   where f = updateOrInsert u row prot

updateOrInsert  :: HSResultUpdater -> HSRow -> HSResultPrototype -> Maybe HSResult -> Maybe HSResult
updateOrInsert  u row prot (Just res)  = Just (u res row prot)
updateOrInsert  u row prot Nothing     = Just (u HSEmptyResult row prot)

joinData :: HSTable t => t -> String -> String -> HSRow -> [HSRow]
joinData tab leftKey rightKey row =  datOrPlaceHolder (Hastistics.Types.lookup rightKey joinVal tab)
                                     where datOrPlaceHolder [] = [toRow (headersOf tab) (take (length (headersOf tab)) (repeat None))]
                                           datOrPlaceHolder xs = xs
                                           joinVal             = fieldValueOf leftKey row

combine             :: HSRow -> HSRow -> HSRow
combine (HSValueRow onehs onefs) (HSValueRow otherhs otherfs)  = HSValueRow (onehs ++ otherhs) (onefs ++ otherfs)

joinRow                     :: HSTable t => t -> String -> String -> HSRow -> HSRow
joinRow tab left right  row =  combine row (head $ joinData tab left right row)


select      :: HSReport -> HSReport
select      = evalReport

updateRow :: HSRow -> HSRow -> HSRow
updateRow (HSValueRow hs fs) row = HSValueRow hs [pack (update c row) | (HSFieldHolder c) <- fs]

updateResults :: HSResult -> [HSRow] -> HSResultPrototype -> HSResultUpdater -> HSResult
updateResults res (r:rs)  p u   = updateResults (u res r p) rs p u
updateResults res []      _ _  = res

--updateResult (HSGroupedResult k prot rs) dat    = HSGroupedResult k prot (Map.alter f (fieldValueOf k dat) rs)


sourceData      :: HSReport -> [HSRow]
sourceData r    = [joinIt row | row <- sourceDat (source r)]
                  where sourceDat (HSTableHolder t)     = dataOf t
                        joinIt                          = setHeader (sheaders r) . joiner r
                        setHeader hs (HSValueRow _ vs)  = HSValueRow hs vs

evalReport :: HSReport -> HSReport
evalReport report = report {result= updateResults (HSSingleResult prot) dat prot updat}
                  where updat       = updater report
                        prot        = proto report
                        dat         = filter predicate (sourceData report)
                        predicate   = constraints report

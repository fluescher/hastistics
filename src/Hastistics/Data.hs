{-# LANGUAGE ExistentialQuantification #-}

module Hastistics.Data where

import qualified Data.Map as Map
import Text.Printf (printf)

data HSValue
   = HSString String
   | HSInt Int
   | HSInteger Integer
   | HSDouble Double
   | None
   deriving(Eq, Ord)

instance Show HSValue where
    show (HSString s)  = s
    show (HSInt i)     = show i
    show (HSInteger i) = show i
    show (HSDouble  d) = printf "%f" d
    show None          = "None"

type Key = String

(+)     :: HSValue -> HSValue -> HSValue
(+) (HSString sa)    (HSString sb) = HSString (sa Prelude.++ sb)
(+) (HSInt ia)       (HSInt ib)    = HSInt (ia Prelude.+ ib)
(+) (HSDouble da)    (HSDouble db) = HSDouble (da Prelude.+ db)
(+) (HSDouble da)    (HSInt ib)    = HSDouble (da Prelude.+ (fromIntegral ib))
(+) (HSInt ia)       (HSDouble db) = HSDouble ((fromIntegral ia) Prelude.+ db)
(+) (HSInteger ia)   (HSInteger ib)= HSInteger (ia Prelude.+ ib)
(+) _               _              = None

(/)     :: HSValue -> HSValue -> HSValue
(/) _               (HSInt 0)      = None
(/) _               (HSDouble 0)   = None
(/) (HSInt ia)      (HSInt ib)     = HSInt (div ia ib)
(/) (HSDouble da)   (HSDouble db)  = HSDouble (da Prelude./ db)
(/) (HSDouble da)   (HSInt ib)     = HSDouble (da Prelude./ (fromIntegral ib))
(/) (HSInt ia)      (HSDouble db)  = HSDouble ((fromIntegral ia) Prelude./ db)
(/) _               _              = None


class (Show f) => HSField f where
    val         :: f -> HSValue
    meta        :: f -> String
    meta   _    =  ""
    update      :: f -> HSRow -> f
    update fi _ =  fi

showField :: (HSField a) => a -> String
showField = show . val

data HSStaticField  = HSStaticField HSValue
instance HSField HSStaticField where
    val (HSStaticField v)   = v
    update f _              = f
instance Show HSStaticField where
    show = showField

data HSValueOfField = HSValueOfField Key HSValue
instance HSField HSValueOfField where
    meta    (HSValueOfField k _)       = "Value of " ++ k
    val     (HSValueOfField _ v)       = v 
    update  (HSValueOfField h None) r  = HSValueOfField h (fieldValueOf h r)
    update  f _                        = f
instance Show HSValueOfField where
    show = showField

data HSAvgField = HSAvgField Key HSValue Int
instance HSField HSAvgField where
    meta    (HSAvgField k _  _  )   = "Average of " ++ k
    val     (HSAvgField _ su cnt)   = su Hastistics.Data./ HSDouble (fromIntegral cnt)
    update  (HSAvgField h su cnt) r = HSAvgField h sm newCnt
                                    where sm     = (Hastistics.Data.+) su (fieldValueOf h r)
                                          newCnt = (Prelude.+) 1 cnt
instance Show HSAvgField where
    show = showField


data HSCountField = HSCountField HSValue
instance HSField HSCountField where
    meta    _                  = "Count"
    val     (HSCountField v)   = v
    update  (HSCountField v) _ = HSCountField (v Hastistics.Data.+ HSInteger 1)

instance Show HSCountField where
    show = showField

data HSSumField = HSSumField Key HSValue
instance HSField HSSumField where
    meta    (HSSumField k _)   = "Sum of " ++ k
    val     (HSSumField _ v)   = v
    update  (HSSumField h v) r = HSSumField h ((Hastistics.Data.+) v (fieldValueOf h r))

instance Show HSSumField where
    show = showField


data HSFieldHolder = forall a. HSField a => HSFieldHolder a

instance Show HSFieldHolder where
    show (HSFieldHolder f) = show f

pack    :: HSField a => a -> HSFieldHolder
pack    = HSFieldHolder

{- |Row in a table. Consists of a list of columns -}
data HSRow      
   = HSValueRow  [Key] [HSFieldHolder] 

instance Show HSRow where
    show = showRow . valuesOf 

{- |Get the values out of a HSRow. -}
valuesOf :: HSRow -> [HSValue]
valuesOf (HSValueRow _ vs)    = [val v | (HSFieldHolder v) <- vs]

toRow        :: [Key] -> [HSValue] -> HSRow
toRow ks vs  = HSValueRow ks [pack (HSStaticField v) | v <- vs]

fieldValueOf :: String -> HSRow -> HSValue
fieldValueOf _ (HSValueRow _ [])                            = None
fieldValueOf _ (HSValueRow [] _)                            = None
fieldValueOf col (HSValueRow (h:hs) ((HSFieldHolder v):vs)) | h == col   = val v
                                                            | otherwise  = fieldValueOf col (HSValueRow hs vs)


{- |Type class defining the interface to a Table implementation. Use this type class if you want to
define your own data sources for the Hastistics framework. -}
class (Show t) => HSTable t where
    headersOf   :: t -> [Key]
    dataOf      :: t -> [HSRow]
    lookup	    :: Key -> HSValue -> t -> [HSRow]
    lookup k v t = [r | r <- (dataOf t), (fieldValueOf k r) == v]

colWidth ::  Int
colWidth = 20

showBorder      :: (Show a) => [a] -> String 
showBorder []     = "+"
showBorder (_:ks) = "+" ++ take (colWidth Prelude.+ 2) (repeat '-')  ++ showBorder ks

showHeader      :: (Show a) => [a] -> String
showHeader ks   = (showBorder ks) ++ "\n" ++ 
                  (showRow    ks) ++ "\n" ++
                  (showBorder ks) 

showRow         :: (Show a) => [a] -> String
showRow []      = "|"
showRow (k:ks)  = "| " ++ v ++ space ++ " " ++ showRow ks
                where   v       = take colWidth (show k)
                        space   = take (colWidth-(length v)) (repeat ' ')

showRows        :: [HSRow] -> String
showRows []     = ""
showRows (r:rs) = showRow (valuesOf r) ++ "\n" ++ showRows rs

showTable       :: HSTable t => t -> String
showTable t     | length (headersOf t) == 0 = ""
                | otherwise                 = showHeader (headersOf t) ++ "\n" ++
                                              showRows (dataOf t) ++ 
                                              showBorder (headersOf t)

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
joinedData (JoinInfo leftKey rightKey (HSTableHolder tab)) row    = datOrPlaceHolder (Hastistics.Data.lookup rightKey joinVal tab)
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


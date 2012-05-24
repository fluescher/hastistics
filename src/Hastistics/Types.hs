{-# LANGUAGE ExistentialQuantification #-}

module Hastistics.Types where

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

type ValueParser = (String -> HSValue)

toString :: ValueParser
toString = HSString

toInt    :: ValueParser
toInt    = HSInt . read

toInteger :: ValueParser 
toInteger = HSInteger . read

toDouble :: ValueParser
toDouble = HSDouble . read


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

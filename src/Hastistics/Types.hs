{-# LANGUAGE ExistentialQuantification #-}

module Hastistics.Types where

import Text.Printf (printf)

data HSValue
   = HSString String
   | HSInt Int
   | HSInteger Integer
   | HSDouble Double
   | None
   deriving(Eq)

instance Ord HSValue where
    (<=) (HSString a)   (HSString b)    = a <= b
    (<=) (HSString a)   (HSInt b)       = a <= show b
    (<=) (HSString a)   (HSInteger b)   = a <= show b
    (<=) (HSString a)   (HSDouble b)    = a <= show b
    
    (<=) (HSInt a)      (HSInt b)       = a <= b
    (<=) (HSInt a)      (HSString b)    = show a <= b
    (<=) (HSInt a)      (HSInteger b)   = (fromIntegral a) <= b
    (<=) (HSInt a)      (HSDouble b)    = (fromIntegral a) <= b
    
    (<=) (HSInteger a)  (HSInteger b)   = a <= b
    (<=) (HSInteger a)  (HSString b)    = (show a) <= b
    (<=) (HSInteger a)  (HSInt b)       = a <= (fromIntegral b)
    (<=) (HSInteger a)  (HSDouble b)    = (fromIntegral a) <= b
    
    (<=) (HSDouble a)   (HSDouble b)    = a <= b
    (<=) (HSDouble a)   (HSString b)    = (show a) <= b
    (<=) (HSDouble a)   (HSInteger b)   = a <= (fromIntegral b)
    (<=) (HSDouble a)   (HSInt b)       = a <= (fromIntegral b)
    
    (<=) None           None            = True
    (<=) None           _               = True
    (<=) _              None            = False

instance Show HSValue where
    show (HSString s)  = s
    show (HSInt i)     = show i
    show (HSInteger i) = show i
    show (HSDouble  d) = printf "%f" d
    show None          = "None"

type Key = String

type ValueParser = (String -> HSValue)

{-| Cast String to HSString -}
toHSString :: ValueParser
toHSString = HSString

{-| Cast String to HSInt -}
toHSInt    :: ValueParser
toHSInt    = HSInt . read

{-| Cast String to HSInteger -}
toHSInteger :: ValueParser 
toHSInteger = HSInteger . read

{-| Cast String to HSDouble -}
toHSDouble :: ValueParser
toHSDouble = HSDouble . read

{-| Cast HSString to String -}
fromHSStringToString :: HSValue -> String
fromHSStringToString (HSString s) = s
fromHSStringToString _            = ""

{-| Cast HSInt to Int -}
fromHSIntToInt :: HSValue -> Int
fromHSIntToInt (HSInt i)    = i
fromHSIntToInt _            = 0

{-| Cast HSInteger to Integer -}
fromHSIntegerToInteger :: HSValue -> Integer
fromHSIntegerToInteger (HSInteger i) = i
fromHSIntegerToInteger _             = 0

{-| Cast HSDouble to Double -}
fromHSDoubleToDouble :: HSValue -> Double
fromHSDoubleToDouble (HSDouble d)  = d
fromHSDoubleToDouble _             = 0

{-| Definitions for arithmetic operations on HSValues  -}
(+)     :: HSValue -> HSValue -> HSValue
(+) (HSDouble da)    (HSDouble db)  = HSDouble (da Prelude.+ db)
(+) (HSDouble da)    (HSInt ib)     = HSDouble (da Prelude.+ (fromIntegral ib))
(+) (HSDouble da)    (HSInteger ib) = HSDouble (da Prelude.+ (fromIntegral ib))
(+) (HSDouble da)    (HSString sb)  = HSString ((show da) Prelude.++ sb)
(+) (HSInt ia)       (HSDouble db)  = HSDouble ((fromIntegral ia) Prelude.+ db)
(+) (HSInt ia)       (HSInt ib)     = HSInt (ia Prelude.+ ib)
(+) (HSInt ia)       (HSInteger ib) = HSInteger ((fromIntegral ia) Prelude.+ ib)
(+) (HSInt ia)       (HSString sb)  = HSString ((show ia) Prelude.++ sb)
(+) (HSInteger ia)   (HSDouble db)  = HSDouble ((fromIntegral ia) Prelude.+ db)
(+) (HSInteger ia)   (HSInt ib)     = HSInteger (ia Prelude.+ (fromIntegral ib))
(+) (HSInteger ia)   (HSInteger ib) = HSInteger (ia Prelude.+ ib)
(+) (HSInteger ia)   (HSString sb)  = HSString ((show ia) Prelude.++ sb)
(+) (HSString sa)    (HSDouble db)  = HSString (sa Prelude.++ (show db))
(+) (HSString sa)    (HSInt ib)     = HSString (sa Prelude.++ (show ib))
(+) (HSString sa)    (HSInteger ib) = HSString (sa Prelude.++ (show ib))
(+) (HSString sa)    (HSString sb)  = HSString (sa Prelude.++ sb)
(+) _                _              = None

(-)     :: HSValue -> HSValue -> HSValue
(-) (HSDouble da)    (HSDouble db)  = HSDouble (da Prelude.- db)
(-) (HSDouble da)    (HSInt ib)     = HSDouble (da Prelude.- (fromIntegral ib))
(-) (HSDouble da)    (HSInteger ib) = HSDouble (da Prelude.- (fromIntegral ib))
(-) (HSInt ia)       (HSDouble db)  = HSDouble ((fromIntegral ia) Prelude.- db)
(-) (HSInt ia)       (HSInt ib)     = HSInt (ia Prelude.- ib)
(-) (HSInt ia)       (HSInteger ib) = HSInteger ((fromIntegral ia) Prelude.- ib)
(-) (HSInteger ia)   (HSDouble db)  = HSDouble ((fromIntegral ia) Prelude.- db)
(-) (HSInteger ia)   (HSInt ib)     = HSInteger (ia Prelude.- (fromIntegral ib))
(-) (HSInteger ia)   (HSInteger ib) = HSInteger (ia Prelude.- ib)
(-) _                _              = None

(/)     :: HSValue -> HSValue -> HSValue
(/) _               (HSDouble 0)    = None
(/) _               (HSInt 0)       = None
(/) _               (HSInteger 0)   = None
(/) (HSDouble 0)    _               = HSDouble 0
(/) (HSInt 0)       _               = HSInt 0
(/) (HSInteger 0)   _               = HSInteger 0
(/) (HSDouble da)   (HSDouble db)   = HSDouble (da Prelude./ db)
(/) (HSDouble da)   (HSInt ib)      = HSDouble (da Prelude./ (fromIntegral ib))
(/) (HSDouble da)   (HSInteger ib)  = HSDouble (da Prelude./ (fromIntegral ib))
(/) (HSInt ia)      (HSDouble db)   = HSDouble ((fromIntegral ia) Prelude./ db)
(/) (HSInt ia)      (HSInt ib)      = HSDouble ((fromIntegral ia) Prelude./ (fromIntegral ib))
(/) (HSInt ia)      (HSInteger ib)  = HSDouble ((fromIntegral ia) Prelude./ (fromIntegral ib))
(/) (HSInteger ia)   (HSDouble db)  = HSDouble ((fromIntegral ia) Prelude./ db)
(/) (HSInteger ia)   (HSInt ib)     = HSDouble ((fromIntegral ia) Prelude./ (fromIntegral ib))
(/) (HSInteger ia)   (HSInteger ib) = HSDouble ((fromIntegral ia) Prelude./ (fromIntegral ib))
(/) _               _               = None

(*)     :: HSValue -> HSValue -> HSValue
(*) _               (HSDouble 0)    = HSDouble 0
(*) _               (HSInt 0)       = HSInt 0
(*) _               (HSInteger 0)   = HSInteger 0
(*) (HSDouble 0)    _               = HSDouble 0
(*) (HSInt 0)       _               = HSInt 0
(*) (HSInteger 0)   _               = HSInteger 0
(*) (HSDouble da)   (HSDouble db)   = HSDouble (da Prelude.* db)
(*) (HSDouble da)   (HSInt ib)      = HSDouble (da Prelude.* (fromIntegral ib))
(*) (HSDouble da)   (HSInteger ib)  = HSDouble (da Prelude.* (fromIntegral ib))
(*) (HSInt ia)      (HSDouble db)   = HSDouble ((fromIntegral ia) Prelude.* db)
(*) (HSInt ia)      (HSInt ib)      = HSDouble ((fromIntegral ia) Prelude.* (fromIntegral ib))
(*) (HSInt ia)      (HSInteger ib)  = HSDouble ((fromIntegral ia) Prelude.* (fromIntegral ib))
(*) (HSInteger ia)  (HSDouble db)   = HSDouble ((fromIntegral ia) Prelude.* db)
(*) (HSInteger ia)  (HSInt ib)      = HSDouble ((fromIntegral ia) Prelude.* (fromIntegral ib))
(*) (HSInteger ia)  (HSInteger ib)  = HSDouble ((fromIntegral ia) Prelude.* (fromIntegral ib))
(*) _               _               = None

class (Show f) => HSField f where
    val         :: f -> HSValue
    meta        :: f -> String
    meta   _    =  ""
    update      :: f -> HSRow -> f
    update fi _ =  fi

{-| get the value of the HSField and cast it to String -}
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

rowFields :: HSRow -> [HSFieldHolder]
rowFields (HSValueRow _ f) = f

rowHeaders :: HSRow -> [Key]
rowHeaders (HSValueRow hs _ ) = hs

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
                        space   = take (colWidth Prelude.- (length v)) (repeat ' ')

showRows        :: [HSRow] -> String
showRows []     = ""
showRows (r:rs) = showRow (valuesOf r) ++ "\n" ++ showRows rs

showTable       :: HSTable t => t -> String
showTable t     | length (headersOf t) == 0 = ""
                | otherwise                 = showHeader (headersOf t) ++ "\n" ++
                                              showRows (dataOf t) ++ 
                                              showBorder (headersOf t)

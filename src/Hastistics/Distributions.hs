module Hastistics.Distributions where

import Hastistics.Data hiding ((/), (+))

header :: [String]
header = ["k", "p"]

data BinominalTable = BinominalTable Integer Double

instance HSTable BinominalTable where
	headersOf _ = header 
	dataOf (BinominalTable n p) = [toHSRow k (binopdf k n p) | k <- [0..n]]  
	lookup _ _ _ = []

instance Show BinominalTable where
    show = showTable

data HypergeometricTable = HypergeometricTable Integer Integer Integer
	  
instance HSTable HypergeometricTable where
	headersOf _ = header
	dataOf (HypergeometricTable m r n) = [toHSRow k (hygepdf k m r n) | k <- [0..r]]
	lookup _ _ _ = []
	 
instance Show HypergeometricTable where
	show = showTable


toHSRow :: Integer -> Double -> HSRow
toHSRow n p = HSValueRow header [pack(HSStaticField(HSInteger n)), pack(HSStaticField(HSDouble p))]

factorial :: Integer-> Double
factorial n 	| n < 0  	= error "negative input"
				| n == 0 	= 1 
				| otherwise	= fromIntegral(product[1..n])

choose :: Integer -> Integer -> Double
n `choose` k 	| k > n 	= 0
				| otherwise	= factorial(n) / (factorial(k) * factorial(n-k))

binopdf :: Integer -> Integer -> Double -> Double
binopdf k n p	| k < 0 || k > n	= 0
	      		| n == 0			= 1
				| otherwise			= (n `choose` k) * p^k * (1-p)^(n-k)

{- |Returns the comulative binomial distribution. -}
binocdf :: Integer -> Integer -> Double -> Double
binocdf k n p 	| k < 0			= 0
				| k >= n		= 1
				| otherwise		= sum [binopdf x n p | x <- [0..k]]

hygepdf :: Integer -> Integer -> Integer -> Integer -> Double
hygepdf k m r n	| n == 0		= 0 
				| otherwise 	= ((r `choose` k) * ((m-r) `choose` (n-k))) / (m `choose` n)

hygecdf k m r n = sum [hygepdf x m r n | x <- [0..k]]

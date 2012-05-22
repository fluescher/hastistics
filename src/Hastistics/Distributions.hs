module Hastistics.Distributions where 

import Hastistics.Data hiding ((/), (+))

data BinominalTable = BinominalTable Integer Double
header :: [String]
header = ["k", "p"]

instance HSTable BinominalTable where
	headersOf _ = header 
	dataOf (BinominalTable n p) = [toHSRow k (binopdf k n p) | k <- [0..n]]  
	lookup _ _ _ = []

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
				| otherwise		= sum [binopdf x n p | x <- [0..(k-1)]]

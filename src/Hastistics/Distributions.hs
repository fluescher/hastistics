module Hastistics.Distributions where

import Hastistics.Types hiding ((/), (+))
import Hastistics.Fields

header :: [String]
header = ["k", "p"]

data BinominalTable = BinominalTable Integer Double

instance HSTable BinominalTable where
	headersOf _ = header 
	dataOf (BinominalTable n p) = [toHSRow k (binopdf k n p) | k <- [0..n]]  

instance Show BinominalTable where
    show = showTable

data HypergeometricTable = HypergeometricTable Integer Integer Integer
	  
instance HSTable HypergeometricTable where
	headersOf _ = header
	dataOf (HypergeometricTable m r n) = [toHSRow k (hygepdf k m r n) | k <- [0..r]]
	lookup _ _ _ = []
	 
instance Show HypergeometricTable where
	show = showTable

data PossionTable = PossionTable Integer Double
	  
instance HSTable PossionTable where
	headersOf _ = header
	dataOf (PossionTable k l) = [toHSRow x (poisspdf  x l) | x <- [0..k]]
	lookup _ _ _ = []
	 
instance Show PossionTable where
	show = showTable

toHSRow :: Integer -> Double -> HSRow
toHSRow n p = HSValueRow header [pack(HSStaticField(HSInteger n)), pack(HSStaticField(HSDouble p))]

factorial :: Integer-> Double
factorial n 	| n < 0  	= error "negative input"
				| n == 0 	= 1 
				| otherwise	= fromIntegral(product[1..n])

choose :: Integer -> Integer -> Double
n `choose` k 	| k > n 	= 0
				| otherwise	= factorial(n) / (factorial(k) * factorial(n Prelude.- k))

{-| Returns the probability of the binominal distribution. -}
binopdf :: Integer -> Integer -> Double -> Double
binopdf k n p	| k < 0 || k > n	= 0
	      		| n == 0			= 1
				| otherwise			= (n `choose` k) * p^k * (1 Prelude.- p)^(n Prelude.- k)

{-| Returns the comulative binomial distribution. -}
binocdf :: Integer -> Integer -> Double -> Double
binocdf k n p 	| k < 0			= 0
				| k >= n		= 1
				| otherwise		= sum [binopdf x n p | x <- [0..k]]
				
{-| Returns the probability of the hypergeometric distribution. -}
hygepdf :: Integer -> Integer -> Integer -> Integer -> Double
hygepdf k m r n	| n == 0		= 0 
				| otherwise 	= ((r `choose` k) * ((m Prelude.- r) `choose` (n Prelude.- k))) / (m `choose` n)

{-| Returns the comulative hypergeometric distribution. -}
hygecdf :: Integer -> Integer -> Integer -> Integer -> Double
hygecdf k m r n = sum [hygepdf x m r n | x <- [0..k]]


{-| Returns the probability of the normal distribution. -}
normpdf :: Double -> Double -> Double -> Double
normpdf x mu sigma = 1 / (sqrt(2 * pi) * sigma) * exp(1) ** ((-((x Prelude.- mu) ** 2) / (2 * sigma ** 2)))

{-| Returns the probability of the poisson distribution. -}
poisspdf :: Integer -> Double -> Double
poisspdf k l = (l ^ k) / (factorial k) * (exp(1) ** (-l))

{-| Returns the comulative poisson distribution. -}
poisscdf :: Integer -> Double -> Double
poisscdf k l = sum [poisspdf x l | x <- [0..k]]

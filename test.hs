{- Lab 1
 Authors: Vlad Alexandru Dragos, Houssam El-Din Boughdadi

 Lab group:
 -}
 ---------------------------------------------

import MeasureTime

power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A -------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k+1


-- B -------------------------
-- power1
power1 :: Integer -> Integer -> Integer
power1 n k 
    | k < 0 = error "power: negative argument"
power1 n k  = product (replicate (fromInteger k) n)


-- C -------------------------
-- power2
power2 :: Integer -> Integer -> Integer
power2 n k
  | k < 0 = error "power: negative argument"
  | even k = power1 (n * n) (div k 2)
  | odd k = n * (power1 n (k - 1))



-- D -------------------------
{- 

<Describe your test cases here>
The test function compares the results from the different power functon using bases from -100 to 100 and exponents from 0 to 100

 -}

-- testFunc range parameters
minExponent :: Integer
minExponent = 0

maxExponent :: Integer
maxExponent = 100


minBase :: Integer
minBase = (-100)

maxBase :: Integer
maxBase = 100
 
 
-- comparePower1
comparePower1 :: Integer -> Integer -> Bool
comparePower1 n k = (power n k) == (power1 n k)
 

-- comparePower2
comparePower2 :: Integer -> Integer -> Bool
comparePower2 n k = (power n k) == (power2 n k)
 

comparePowers :: Integer -> Integer -> Bool
comparePowers n k = (comparePower1 n k) == (comparePower2 n k)

-- Test functions: 
testFunc :: Bool
testFunc = not (elem False [comparePowers n k | n <- [minBase..maxBase], k <- [minExponent..maxExponent]]) 
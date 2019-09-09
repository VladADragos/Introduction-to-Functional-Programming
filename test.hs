{- Lab 1
   Authors:
   Lab group:
 -}

import MeasureTime
---------------------------------------------
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)


stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k+1





power1 :: Integer -> Integer -> Integer
power1 n k  = product (replicate (fromInteger k) n)


power2 :: Integer -> Integer -> Integer
power2 n k
  | even k = power1 (n * n) (div k 2)
  | odd k = n * (power1 n (k - 1))



  

-- test 0,1, odd anv even
-- number should be uint

boolToStr :: Bool -> String
boolToStr True = "True"
boolToStr False = "False"

-- test1 = putStrLn "Hello" putStrLn "World"



comparePower1 :: Integer -> Integer -> Bool
comparePower1 n k = (power n k) == (power1 n k)


comparePower2 :: Integer -> Integer -> Bool
comparePower2 n k = (power n k) == (power2 n k)


comp :: Integer -> Integer -> Bool
comp n k = (comparePower1 n k) == (comparePower2 n k)


minimumm = 0
maximumm = 100

test :: Bool
test = not (elem False [comp n k | n <- [-100..100], k <- [0..100]]) 


{- Lab 1
   Authors:
   Lab group:
 -}
---------------------------------------------
power :: Int -> Int -> Int
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)




-- A -------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Int -> Int -> Int
stepsPower n k = n-1


-- B -------------------------
-- power1

-- power2 = undefined

-- C -------------------------
-- power2

power3 = undefined

-- D -------------------------
{- 

<Describe your test cases here>

 -}

-- comparePower1
-- comparePower1 = undefined

-- comparePower2
-- comparePower2 = undefined

-- Test functions: 


-- length len()¨

-- list !! index  to get element in array
-- head list to get first element
-- last list to get last element

-- list take 3 gets first 3 elements

-- list drop 3 gets removes first 3 elements



createList :: Int -> Int -> [Int]
createList length element = take length (repeat element) 


power1 :: Int -> Int -> Int
power1 n k  = product (createList k n)


power2 :: Int -> Int -> Int
power2 n k
  | k == 0 = 1
  | odd k = n * (power2 n (k - 1))
  | not (odd k)= power2 (n * n * n) (div k 2)



  

-- test 0,1, odd anv even
-- number should be uint

boolToStr :: Bool -> String
boolToStr True = "True"
boolToStr False = "False"

test1 = putStrLn "Hello" putStrLn "World"



comparePower1 :: Int -> Int -> Bool
comparePower1 n k = (power n k) == (power1 n k)


comparePower2 :: Int -> Int -> Bool
comparePower2 n k = (power n k) == (power2 n k)
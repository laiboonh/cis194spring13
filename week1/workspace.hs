{-# OPTIONS_GHC -Wall #-}

i :: Int
i = -7

--n :: Integer
--n = 1234567890987654321987340982334987349872349874534

hailstone :: Integer -> Integer
hailstone n
  | isEven n = n `div` 2
  | otherwise      = 3*n + 1

foo :: Integer -> Integer
foo 0 = 16
foo 1
  | "Haskell" > "C++" = 3
  | otherwise         = 4
foo n
  | n < 0            = 0
  | n `mod` 17 == 2  = -43
  | otherwise        = n + 3

{-
foo (-3)? 0
foo 0? 16
foo 1? 3
foo 36? (-43)
foo 38? 41
-}

isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0

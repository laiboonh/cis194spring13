{-# OPTIONS_GHC -Wall #-}

i :: Int
i = -7

n :: Integer
n = 1234567890987654321987340982334987349872349874534

hailstone :: Integer -> Integer
hailstone n'
  | n' `mod` 2 == 0 = n' `div` 2
  | otherwise      = 3*n' + 1

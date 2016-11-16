{-# OPTIONS_GHC -Wall #-}

module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

data Stream a =
  Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a stream) = a : (streamToList stream)

instance Show a => Show (Stream a) where
  show stream = show (take 20 (streamToList stream))

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a stream) = Cons (f a) (streamMap f stream)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = streamMap r (streamFromSeed (+1) 1)
  where r x | odd x = 0
            | otherwise = r (x `div` 2::Integer) + 1

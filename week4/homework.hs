{-# OPTIONS_GHC -Wall #-}

import Data.List ((\\))

fun1 :: [Integer] -> Integer
fun1 =  foldr (*) 1 . map (+(-2)) . filter (even)

fun2 :: Integer -> Integer
fun2 = sum . filter (even) . takeWhile (>1) . iterate collatz where
  collatz n
    | even n = n `div` 2
    | otherwise  = 3*n + 1

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr makeTree Leaf

makeTree :: a -> Tree a -> Tree a
makeTree a Leaf = Node 0 Leaf a Leaf
makeTree a (Node _ l m r)
   | height l <= height r = let left = makeTree a l in Node (height (left) + 1) left m r
   | otherwise = let right= makeTree a r in Node (height (right) + 1) l m right
  where
    height Leaf = -1
    height (Node h' _ _ _) = h'

xor :: [Bool] -> Bool
xor xs = foldr fun False xs where
  fun x acc
    | x == True = not acc
    | otherwise = acc

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f acc xs = foldr fun id xs acc
    where fun x g acc' = g (f acc' x)

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =  map (\x -> 2*x+1) $ [1..n] \\ (out n)

out :: Integer -> [Integer]
out n = [i+j+(2*i*j) | j <- [1..n], i <- [1..n], i+j+(2*i*j) <= n]

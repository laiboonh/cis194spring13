{-# OPTIONS_GHC -Wall #-}

import Data.List (sort,group)

skips :: [a] -> [[a]]
skips xs = go 1 [] where
  go n acc
    | n > (length xs) = acc
    | n == 1 = xs : (go (n+1) acc)
    | n < 1 = []
    | otherwise = (everyN n xs) : (go (n+1) acc)

everyN :: Int -> [a] -> [a]
everyN n xs =
  case (drop (n-1) xs) of
    y:ys -> y : (everyN n ys)
    [] -> []

localMaxima :: [Integer] -> [Integer]
localMaxima ys = go [] ys where
  go acc (x1:x2:x3:xs)
    | x2 > x1 && x2 > x3 = go (acc++[x2]) (x2:x3:xs)
    | otherwise = go acc (x2:x3:xs)
  go acc _ = acc

histogram :: [Integer] -> String
histogram xs = accumulateLines $ prepare (group $ sort xs) 0 []

prepare :: [[Integer]] -> Integer -> [Integer] -> [Integer]
prepare lys@(lxs@(x:_):ys) n acc
  | n == x = prepare ys (n+1) (acc ++ [(fromIntegral $ length lxs)])
  | n /= x = prepare lys (n+1) (acc ++ [0])
  | otherwise = error "unexpected"
prepare [] n acc
  | n /= 10 = prepare [] (n + 1) (acc ++ [0])
  | otherwise = acc
prepare ([] : _) _ _ = error "unexpected"

accumulateLines :: [Integer] -> String
accumulateLines ys = unlines $ go [] ys (maximum ys) where
  go acc [] _ = acc
  go acc _ 0 = acc ++ ["=========="] ++ ["0123456789"]
  go acc xs maxN = go acc' xs' (maxN-1) where
    acc' = acc ++ [(writeLine xs)]
    xs' = decrementLine xs

decrementLine :: [Integer] -> [Integer]
decrementLine xs = map fun xs where
  fun x
    | x == maxNum = x - 1
    | otherwise = x
  maxNum = maximum xs

writeLine :: [Integer] -> String
writeLine xs = map fun xs where
  fun x
    | x == maxNum = '*'
    | otherwise = ' '
  maxNum = maximum xs

{-# OPTIONS_GHC -Wall #-}

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | fst divMod10 == 0 = [snd divMod10]
  | otherwise = toDigits (fst divMod10) ++ [snd divMod10]
  where divMod10 = n `divMod` 10

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | fst divMod10 == 0 = [snd divMod10]
  | otherwise =  [snd divMod10] ++ (toDigitsRev $ fst divMod10)
  where divMod10 = n `divMod` 10

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = foldr fun [] xs where
  fun :: Integer -> [Integer] -> [Integer]
  fun n acc
    | even $ length acc = n : acc
    | otherwise = (n*2) : acc

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ concat $ map toDigits xs

checkSum :: Integer -> Integer
checkSum = sumDigits . doubleEveryOther . toDigits

validate :: Integer -> Bool
validate n = ((checkSum n) `rem` 10) == 0

main :: IO()
main = do
  print $ toDigits 1234 == [1,2,3,4]
  print $ toDigitsRev 1234 == [4,3,2,1]
  print $ toDigits 0 == []
  print $ toDigits (-17) == []
  print $ doubleEveryOther [8,7,6,5] == [16,7,12,5]
  print $ doubleEveryOther [1,2,3] == [1,4,3]
  print $ sumDigits [16,7,12,5] == 22
  print $ validate 4012888888881881 == True
  print $ validate 4012888888881882 == False

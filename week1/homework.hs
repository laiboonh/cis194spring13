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

main :: IO()
main = do
  print $ toDigits 1234 == [1,2,3,4]
  print $ toDigitsRev 1234 == [4,3,2,1]
  print $ toDigits 0 == []
  print $ toDigits (-17) == []

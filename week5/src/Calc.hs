{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser (parseExp)
import qualified StackVM as S (StackExp(..), Program)

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add left right) = (eval left) + (eval right)
eval (Mul left right) = (eval left) * (eval right)

evalStr :: String -> Maybe Integer
evalStr xs =
  case (parseExp Lit Add Mul xs) of
    Nothing -> Nothing
    Just expr -> Just $ eval expr

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr S.Program where
  lit x = [S.PushI x]
  add x y = x ++ y ++ [S.Add]
  mul x y = x ++ y ++ [S.Mul]

compile :: String -> Maybe S.Program
compile xs = parseExp lit add mul xs

instance Expr Integer where
  lit x = x
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit x
    | x <= 0 = False
    | otherwise = True
  add x y = x || y
  mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit x = MinMax x
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 (x+y `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 (x*y `mod` 7)

instance Expr ExprT where
  lit x = Lit x
  add x y = Add x y
  mul x y = Mul x y



testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer

testBool :: Maybe Bool
testBool = testExp :: Maybe Bool

testMM :: Maybe MinMax
testMM = testExp :: Maybe MinMax

testSat :: Maybe Mod7
testSat = testExp :: Maybe Mod7

main :: IO()
main = do
  print $ eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20
  print $ evalStr "(2+3)*4"
  print $ eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))

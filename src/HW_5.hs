{-# LANGUAGE FlexibleInstances #-}

module HW_5 where

import ExprT
import Parser
import qualified Data.Map as M
import Control.Applicative

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

data VarExprT = Lit' Integer
              | Add' VarExprT VarExprT
              | Mul' VarExprT VarExprT
              | Var String
    deriving(Show, Eq)

-- Exercise 1


eval :: ExprT -> Integer
eval expr = case expr of 
                (Lit x)     -> x
                (Add e1 e2) -> eval e1 + eval e2
                (Mul e1 e2) -> eval e1 * eval e2


-- Exercise 2


evalStr :: String -> Maybe Integer
evalStr str = case (parseExp Lit Add Mul str) of
                Just expr -> Just (eval expr)
                Nothing   -> Nothing 


-- Exercise 3


class Expr a where
    lit :: Integer -> a 
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit x = Lit x
    add (Lit x) (Lit y) = Lit (x+y)
    mul (Lit x) (Lit y) = Lit (x*y)


reify :: ExprT -> ExprT
reify = id


-- Exercise 4


instance Expr Integer where
    lit x = x
    add x y = x+y
    mul x y = x*y

instance Expr Bool where
    lit x
        | x <= 0    = False
        | otherwise = True
    add x y = x || y
    mul x y = x && y

instance Expr MinMax where
    lit x = MinMax x
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
    lit x = Mod7 x
    add (Mod7 x) (Mod7 y) = Mod7 ((x+y) `mod` 7)
    mul (Mod7 x) (Mod7 y) = Mod7 ((x*y) `mod` 7)


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7


-- Exercise 6


class HasVars a where
    var :: String -> a


instance HasVars VarExprT where
    var s = Var s  


instance Expr VarExprT where
    lit x = Lit' x
    add (Lit' x) (Lit' y) = Lit' (x+y)
    mul (Lit' x) (Lit' y) = Lit' (x*y)

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var s = M.lookup s

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit x = partialLit x
                            where
                                partialLit :: Integer -> M.Map String Integer -> Maybe Integer
                                partialLit x kv = Just x

    add f1 f2 = partialAdd f1 f2
                            where
                                partialAdd :: (M.Map String Integer -> Maybe Integer) 
                                           -> (M.Map String Integer -> Maybe Integer) 
                                           -> M.Map String Integer -> Maybe Integer
                                partialAdd f1 f2 kv = liftA2 (+) (f1 kv) (f2 kv)

    mul f1 f2 = partialMul f1 f2
                            where
                                partialMul :: (M.Map String Integer -> Maybe Integer) 
                                           -> (M.Map String Integer -> Maybe Integer) 
                                           -> M.Map String Integer -> Maybe Integer
                                partialMul f1 f2 kv = liftA2 (*) (f1 kv) (f2 kv)


withVars :: [(String, Integer)]
            -> (M.Map String Integer -> Maybe Integer)
            -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
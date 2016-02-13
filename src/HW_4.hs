
module HW_4 where

    
-- Exercise 1


fun1 :: [Integer] -> Integer
fun1 lst = foldl (*) 1 $ map (+ (-2)) $ filter even lst

fun2 :: Integer -> Integer
fun2 n = foldl (+) 0 $ takeWhile (/=1) $filter(\x -> even x || x == 1) $ iterate (\x -> if even x then x `div` 2 else  3*x+1) n


-- Exercise 2


data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree (x:xs) = foldr func Leaf (x:xs)

func :: a -> Tree a -> Tree a
func x (Leaf) = Node 0 (Leaf) x (Leaf) 
func x (Node 0 (Leaf) n (Leaf)) = Node 1 (func x (Leaf)) n (Leaf) 
func x (Node 1 (Node 0 (Leaf) nl (Leaf)) n (Leaf)) = Node 1 (Node 0 (Leaf) nl (Leaf)) n (func x (Leaf))
func x (Node h l n r)
        | isBal leftSide = leftSide
        | isBal rightSide = rightSide
        | otherwise = leftSide
        where
            leftSide  = Node (1 + (max (height (func x l)) (height r))) (func x l) n r
            rightSide = Node (1 + (max (height l) (height (func x r)))) l n (func x r)

 

height :: Tree a -> Integer 
height Leaf = 0
height (Node h _ _ _) = h

isBal :: Tree a -> Bool
isBal Leaf = True
isBal (Node 0 (Leaf) n (Leaf)) = True
isBal (Node h l n r)
        | abs (height l - height r) <= 1 && (isBal l) && (isBal r) = True
        | otherwise = False 


-- Exercise 3


xor :: [Bool] -> Bool
xor [] = False
xor xs = foldr (\x y -> if x==y then False else True) False xs


map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\u v -> f u : v ) []


-- Exercise 4


sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x+1) $ filter (\x -> not (x `elem` lst)) [1,2..n]  
                    where lst = map (\(x,y) -> x+y+2*x*y) $filter (\(x,y) -> (x <= y) && (x+y+2*x*y <= n)) (cartProd [1,2..n] [1,2..n])

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
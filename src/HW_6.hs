
-- Exercise 1

fib :: Integer -> Integer
fib n
        | n <= (-1) = 0
        | n == 1    = 0
        | n == 2    = 1
        | otherwise = fib (n-1) + fib (n-2)



fibs1 :: [Integer]
fibs1 = map fib [1,2..]


-- Exercise 2


fibs2 :: [Integer]
fibs2 = 0:1:zipWith (+) fibs2 (tail fibs2)


-- Exercise 3


data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
    show s = show $ take 20 (lst s) 
                where
                    lst (Cons x (y)) = x : lst y 

streamToList :: Stream a -> [a]
streamToList (Cons x y) = x : streamToList y


-- Exercise 4


streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)


streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x (y)) = Cons (f x) (streamMap f y)


streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))


-- Exercise 5


nats :: Stream Integer
nats = streamFromSeed (+1) 0


ruler :: Stream Integer
ruler = listToStream $ map f [1,2..] 

f n = fst.last $ filter (\(x,y) -> y == 0) $ zip [0,1..n]  $ map ((n `mod`) . (2^)) [0,1..n]  

listToStream :: [Integer] -> Stream Integer
listToStream (x:xs) = Cons x (listToStream xs)
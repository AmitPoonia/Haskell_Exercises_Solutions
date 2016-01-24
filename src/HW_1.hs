
-- Exercise 1

toDigits :: Integer -> [Integer]
toDigits n
        | n > 0     = revList (toDigitsRev n)
        | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
        | n > 0     = (n `mod` 10) : toDigitsRev (n `div` 10)
        | otherwise = []


revList :: [a] -> [a] -- To reverse a list
revList [] = []
revList (x:xs) = revList xs ++ [x]


-- Exercise 2

doubleEveryOther:: [Integer] -> [Integer]
doubleEveryOther n = revList (doubleIt (revList n))

doubleIt :: [Integer] -> [Integer]
doubleIt [] = []
doubleIt (x:[]) = (x:[])
doubleIt (x:y:xs) = x : (2*y) : (doubleIt xs)


-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sumIt (toDigits x) + (sumDigits xs)

sumIt :: [Integer] -> Integer
sumIt [] = 0 
sumIt (x:xs) = x + sumIt xs 


-- Exercise 4

validate :: Integer -> Bool
validate n
    | (sumDigits  (doubleEveryOther (toDigits n))) `mod` 10 == 0  = True
    | otherwise                                                   = False


-- Exercise 5

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
        | n <= 0    = []
        | n == 1    = [(a,b)]
        | otherwise = (hanoi (n-1) a c b) ++ [(a, b)] ++ (hanoi (n-1) c b a)
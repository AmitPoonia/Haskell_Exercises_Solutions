
module HW_3 where
import Data.List


-- Exercise 1


skips :: [a] -> [[a]]
skips [] = []
skips lst = case (lst, (length lst)) of
                (ls,lg) -> map f (zip ([1,2..lg])  (replicate lg ls))


f :: (Int, [a]) -> [a]  
f (_, []) = []
f (0, lst) = lst
f (n, lst)
        |(length lst) > (n-1)  =  (lst !! (n-1)) : f (n, ((snd.splitAt n) lst))
        | otherwise            = [] 




-- Exercise 2

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima (x:y:[]) = []
localMaxima (x:y:z:xs)
        | y > x && y > z  = y : localMaxima (y:z:xs)
        | otherwise       = localMaxima (y:z:xs) 




-- Exercise 3

histogram :: [Integer] -> String
histogram lst = func (getSplits lst) ++ "==========\n0123456789\n"

func :: [(Integer,Integer)] -> String
func [] = ""
func lst
        | and (map (\(u,v) -> (v == 0)) lst) == True     =  ""
        | otherwise =  func (map (\(u,v) -> (u, max 0 (v-1))) lst ) ++ f1 (checkSplits [0,1..9] lst)


f1 :: [(Integer, Integer)] -> String
f1 [] = "\n"
f1 ((x,y):xs)
        | y > 0     = "*" ++ f1 xs
        | otherwise = " " ++ f1 xs

checkSplits :: [Integer] -> [(Integer, Integer)] -> [(Integer, Integer)]
checkSplits [] [] = []
checkSplits (n:ns) [] = (n,0) : checkSplits ns []
checkSplits (n:ns) ((x,y):xs)
        | n == x    = (x,y) : checkSplits ns xs
        | otherwise = checkSplits (n:ns) ((n,0):(x,y):xs)

getSplits :: [Integer] -> [(Integer, Integer)]
getSplits [] = []
getSplits lst = map (\lst'@(x:xs) -> (x, (genericLength $ lst'))) (group.sort $ lst)



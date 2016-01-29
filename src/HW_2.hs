
module HW_2 where

import Log

-- Exercise 1


parseMessage :: String -> LogMessage
parseMessage x = case (words x) of
        ("I":icode:rest)         -> LogMessage Info (read icode) (unwords rest)
        ("E":ecode1:ecode2:rest) -> LogMessage (Error (read ecode1)) (read ecode2) (unwords rest)
        ("W":wcode:rest)         -> LogMessage Warning (read wcode) (unwords rest)
        _                        -> Unknown (x)

parse :: String -> [LogMessage]
parse s = case (lines s) of
        []     -> []
        (x:[]) -> [parseMessage x]
        (x:xs) -> (parseMessage x) : parse (unlines xs) 


-- Exercise 2


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tr             = tr
insert (LogMessage x y z) (Leaf)  = Node (Leaf) (LogMessage x y z) (Leaf)
insert (LogMessage x y z) (Node lTr lMs rTr)
        | y < (tStamp lMs) = Node (insert (LogMessage x y z) lTr) lMs rTr
        | y > (tStamp lMs) = Node lTr lMs (insert (LogMessage x y z) rTr)
        | otherwise        = (Leaf)


tStamp :: LogMessage -> TimeStamp
tStamp x = case x of
        (Unknown _)        -> 0
        (LogMessage _ y _) -> y


-- Exercise 3


build :: [LogMessage] -> MessageTree
build exr = case exr of
        []     -> Leaf
        (x:xs) -> insert x (build xs)


-- Exercise 4


inOrder :: MessageTree -> [LogMessage]
inOrder exr = case exr of
        Leaf               -> []
        (Node lTr lMs rTr) -> (inOrder lTr) ++ [lMs] ++ (inOrder rTr)


-- Exercise 5


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong exr = case exr of 
        [] -> []
        _  -> fltr (inOrder (build exr))

fltr :: [LogMessage] -> [String]
fltr [] = []
fltr ((LogMessage (Error x) _ z):xs)
        | x >= 50   = z : (fltr xs)
        | otherwise = fltr xs
fltr (_:xs) = fltr xs
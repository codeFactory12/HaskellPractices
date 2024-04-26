module Main where

import Control.Monad (liftM)

data Tree a = Null | Node a (Tree a) (Tree a)

-- show in-order tree
instance (Show a) => Show (Tree a) where
    show Null = "(null)"
    show (Node val Null Null) = show val
    show (Node val Null t2) = show val ++ " " ++ show t2
    show (Node val t1 Null) = show t1 ++ " " ++ show val
    show (Node val t1 t2) = show t1 ++ " " ++ show val ++ " " ++ show t2

-- Build tree from [[Int]] lista de listas
treefromarr :: [[Int]] -> Tree Int
treefromarr arr = treefromarr' arr 1

treefromarr' :: [[Int]] -> Int -> Tree Int
treefromarr' arr index
    | index == -1 = Null
    | otherwise =
        Node index
        -- subNodes build
            (treefromarr' arr (arr !! (index-1) !! 0)) -- izq
            (treefromarr' arr (arr !! (index-1) !! 1)) -- der

-- Lines into [String] for nlines
readninput :: IO [String]
readninput = do
    line <- getLine
    let nlines = read line :: Int
    sequence [getLine | _ <- [1..nlines]]

doswap :: Tree a -> Int -> Tree a
doswap Null _ = Null
doswap (Node v t1 t2) depth
    | depth <= 1 = Node v t2 t1
    | otherwise = Node v (doswap t1 (depth-1)) (doswap t2 (depth-1))

swapprintloop :: Tree Int -> [[Int]] -> IO ()
swapprintloop _ [] = return ()
swapprintloop tree ([]:xss) = do
    print tree
    swapprintloop tree xss
swapprintloop tree ((x:xs):xss) = do
    swapprintloop (doswap tree x) (xs:xss)

main :: IO ()
main = do
    --leemos informacion
    lines <- readninput
    -- de lines a [Int]
    let childlist = map (map (read :: String -> Int) . words) lines
    let nchilds = length childlist
    -- informacion a tree (alimentar tree)
    let tree = treefromarr childlist


    lines <- readninput
    let swapbases = map (read :: String -> Int) lines
    let swaps = map (\n -> takeWhile (\x -> x <= nchilds) [n*i | i <- [1..]]) swapbases
    
    swapprintloop tree swaps

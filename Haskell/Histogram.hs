module Histogram where
import Data.List

histograma :: [Int] -> IO ()
histograma xs = do
    let groupedData = groupByCount xs
    putStr (formatHistogram [0..9] groupedData)
    putStr "\n"
    putStr printHistogram
    putStr "\n"

groupByCount :: [Int] -> [Int]
groupByCount xs = [counter x xs | x <- [0..9]]

counter :: Int -> [Int] -> Int
counter _ [] = 0
counter a (x:xs) | a == x    = 1 + counter a xs
                 | otherwise = counter a xs

formatHistogram :: [Int] -> [Int] -> String
formatHistogram nums counts =
    unlines $ reverse $ transpose $ map (formatRow nums) counts

formatRow :: [Int] -> Int -> String
formatRow nums count = concatMap (\num -> if num > 0 then "* " else "  ") (replicate count 1 ++ replicate (length nums - count) 0)

printHistogram :: String
printHistogram = "==================\n0123456789"

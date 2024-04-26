module MissingNumbers where

import Data.List (sort,group)

countFrequency :: [Int] -> [(Int, Int)]
countFrequency = map (\xs -> (head xs, length xs)) . group . sort

missingNumbers :: [Int] -> [Int] -> [Int]
missingNumbers listA listB = sort [x | (x, freqB) <- countFrequency listB, let freqA = lookup x (countFrequency listA),maybe True (< freqB) freqA]

main :: IO ()
main = do
    _ <- getLine 
    listA <- fmap (map read . words) getLine
    _ <- getLine
    listB <- fmap (map read . words) getLine
    let result = missingNumbers listA listB
    putStrLn (unwords (map show result))
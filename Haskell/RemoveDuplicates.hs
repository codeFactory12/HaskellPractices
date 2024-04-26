module RemoveDuplicates where

remDuplicates :: String -> String
remDuplicates [] = []
remDuplicates (x:xs) = x : remDuplicates (filter (/= x) xs)
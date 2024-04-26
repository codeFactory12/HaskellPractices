module PrimeSUm where
    
    elimina :: Int -> [Int] -> [Int]
    elimina _ [] = []
    elimina n xs = [x | x <- xs, x `mod` n /= 0 ]

    criba :: [Int] -> [Int]
    criba [] = []
    criba (x:xs) = x : elimina x xs
    
    esPrimo :: Int -> Bool
    esPrimo n = head (dropWhile (<n) (criba[2..])) == n

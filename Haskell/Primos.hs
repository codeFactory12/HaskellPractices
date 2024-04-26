esPrim :: Int -> Bool
esPrim n | n > 0 = [d | d <- [1..n], n `mod` d == 0] == [1,n]
         | otherwise = False

getPrimos :: Int -> [Int]
getPrimos n = filter esPrim [1..n]

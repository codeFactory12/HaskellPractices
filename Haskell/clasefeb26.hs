module Example where
    suma :: Num a => a -> a -> a
    suma a b = a + b

-- Recursividad con factorial 
    fact :: Int -> Int
    fact 0 = 1
    fact n = n * fact(n - 1)
    
    mylength :: [a] -> Int
    mylength [] = 0
    mylength (_:xs) = 1 + mylength xs 

    insertVal :: Ord a => a -> [a] -> [a]
    insertVal x [] = [x]
    insertVal x (y:ys) | x <= y = x : y : ys
                       | otherwise = y : insertVal x ys
    
    myzip :: [a] -> [b] -> [(a,b)]
    myzip [] _ = []
    myzip _ [] = []
    myzip (x:xs) (y:ys) = (x, y): myzip xs ys  

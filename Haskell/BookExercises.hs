
module BookExercises where
import Data.List

    --CHAPTER 3 ANSWERS
    second xs = head (tail xs)
    -- [a] -> a
    swap (x, y) = (y,x)
    -- (a, b) -> (b, a)
    pair x y =(x ,y)
    -- a -> b -> (a, b)
    double x = x * 2
    -- a -> a
    palindrome xs = reverse xs == xs
    -- [a] -> Bool
    twice f x = f (f x)
    -- (a -> a) -> a

    {-5. ¿Por qué no es factible en general que los tipos de funciones sean instancias de la clase Eq?
    No es práctico considerar que los tipos de funciones sean comparables por igualdad. 
    La razón principal es que decidir si dos funciones son iguales en general resulta ser 
    un problema sin solución. La clase de igualdad (Eq) en Haskell se utiliza para comprobar 
    si dos valores son iguales-}

    -- CHAPTER 4 ANSWERS
    -- 5.
    formalised :: Bool-> Bool -> Bool
    formalised a b = if a then (if b then True else False) else False

    -- 6.
    alternative :: Bool -> String -> String
    alternative a b = if not a then "False" else b
    --7.
    mult :: Int -> Int -> Int -> Int
    mult = \x -> (\y -> (\z -> x * y *z))
    --8.
    luhnDouble :: Int -> Int
    luhnDouble x
                | doubled > 9 = doubled - 9
                | otherwise   = doubled
                where doubled = x * 2

    luhn :: Int -> Int -> Int -> Int -> Bool
    luhn a b c d =
        let total = luhnDouble a + b + luhnDouble c + d
        in total `mod` 10 == 0
    -- CHAPTER 5 ANSWERS
    -- 6
    factors :: Int -> [Int]
    factors n = [x | x <- [1..n-1], n `mod` x == 0]

    perfects :: Int -> [Int]
    perfects limit = [x | x <- [1..limit], x == sum (factors x)]

    -- 7
    concatenated = concat [[(x, y) | y <- [3,4]] | x <- [1,2]]
    -- 8
    positions' :: Eq a => a -> [a] -> [Int]
    positions' x xs = case find (\y -> y == x) xs of
                    Just elem -> [i | (i, y) <- zip [0..] xs, y == elem]
                    Nothing   -> []
    -- 9
    scalarproduct :: [Int] -> [Int] -> Int
    scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

    -- CHAPTER 6 ANSWERS
    -- 5
    -- 6
    -- 6.a
        and :: [Bool] -> Bool
        and [] = False
        and [x] = x
        and (x:xs) = z && and xs
    -- 6.b 
        concat :: [[a]] -> [a]
        concat [] = []
        concat (x:xs) = x ++ concat xs
    -- 6.c
        replicate :: Int -> a -> [a]
        replicate 0 _ = []
        replicate n x = x : replicate (n - 1) x
    -- 6.e 
        elem :: Eq a => a -> [a] -> Bool
        elem _ [] = False
        elem y (x:xs) = (y == x) || elem' y xs


    -- 7
        merge :: Ord a => [a] -> [a] -> [a]
        merge [] ys = ys
        merge xs [] = xs
        merge (x:xs) (y:ys)
                | x <= y    = x : merge xs (y:ys)
                | otherwise = y : merge (x:xs) ys

    -- 8
        msort :: Ord a => [a] -> [a]
        msort [] = []
        msort [x] = [x]
        msort xs = merge (msort firstHalf) (msort secondHalf)
            where
                (firstHalf, secondHalf) = halve xs

        halve :: [a] -> ([a], [a])
        halve xs = splitAt (length xs `div` 2) xs

    -- 9
    -- 9.a 
        sumList :: Num a => [a] -> a
        sumList [] = 0
        sumList (x:xs) = x + sumList xs
    -- 9.b 
        takeFromStart :: Int -> [a] -> [a]
        takeFromStart _ [] = []
        takeFromStart n (x:xs)
            |n <= 0    = []
            |otherwise = x : takeFromStart (n - 1) xs
    -- 9.c 
        lastElement :: [a] -> a
        lastElement [x] = x
        lastElement (_:xs) = lastElement xs
    -- CHAPTER 7 ANSWERS
    --6
    -- 6.a
    chop8 :: [Bit] -> [[Bit]]
    chop8 = unfold null (take 8) (drop 8)
 
    -- 6.b
    map :: (a -> b) -> [a] -> [b]
    map f = unfold null (f . head) tail
 
    -- 6.c 
    iterate :: (a -> a) -> a -> [a]
    iterate f = unfold (\_ -> False) id f
    --7
    --8
    --9
    altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
    altMap _ _ [] = []
    altMap f g (x:xs) = f x : altMap g f xs








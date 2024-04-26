module Ejemplo where

import Data.Char(ord, chr, isUpper)
-- no es bueno poner sum pq ya tiene haskel un operador de suma
suma:: Int -> Int -> Int
suma a b = a + b

suma5' :: Float -> Float -> Float
suma5' a b = a + b

unChar :: Char
unChar = 'A'
               -- Tupla
tupla :: Char -> (Char, Int)
tupla x = (x , ord x)

sumatupla :: (Int, Int, Char) -> Int
sumatupla (a, b, c) = a + b

--listas
-- se pone generalmente xs o ys para una lista
sumaLista :: [Int] -> [Int]
sumaLista xs = xs

sumaLista1 :: [Int] -> Int
sumaLista1 xs = sum (xs)

sumaLista2 :: [Int] -> Int
sumaLista2 = sum

lista3:: [Int]
lista3 = [1..30]

esPar :: Int -> Bool
esPar n = mod n 2 == 0

filtrarPares :: [Int] -> [Int]
filtrarPares xs = filter esPar xs

esPar1 :: Int -> Bool
esPar1 n = even n

filtrarPares1 :: [Int] -> [Int]
filtrarPares1 xs = filter esPar xs

mysplitAt :: Int -> [Int] -> ([Int], [Int])
mysplitAt n xs = (take n xs, drop n xs)

myAbs :: Int -> Int
myAbs n = if n >= 0 then n else -n
--Guarded Equation
myAbs' :: Int -> Int
myAbs' n | n >= 0 = n
         | otherwise = -n

validPositiveNumber :: Int -> Int
validPositiveNumber n
  | n < 0 = -1
  | n == 0 = 0
  | otherwise = 1
--Guarded Equation
validPositiveNumber' :: Int -> Int
validPositiveNumber' n | n < 0 = -1
                       | n == 0 = 0
                       | otherwise = 1

--Pattern matching
negar :: Bool -> Bool
negar True = False
negar False = True


esUnoDos :: Int -> String
esUnoDos 1 = "one"
esUnoDos 2 = "dos"
esUnoDos _ = "no se que es"

-- filter (\x -> mod x 2 == 0) lista

--lambda funtion
add:: Int -> (Int-> Int)
add x y = x + y

myFuntions :: [Int -> Int -> Int]
myFuntions = [(+), (-), (*), div]

getFuntion :: Char -> (Int -> Int -> Int)
getFuntion e | e == '+' = head myFuntions
             | e == '-' = myFuntions !! 1
             | e == '*' = myFuntions !! 2
             | otherwise = myFuntions !! 3

{-
myExp :: Char -> Int
myExp e = getFuntion e
-}
-- curried funtion
-- ((multThree 1) 3) 4

multThree :: Int -> (Int -> (Int -> Int))
multThree x y z = z * y * z

-- [x^2 | x E [1..5]]
-- [x^2 | x <- [1..5]]

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort menor ++ [x] ++ qsort mayor
                where
                    menor = [ me | me <- xs, me < x ]
                    mayor = [ ma | ma <- xs, ma >= x ]

esPrim :: Int -> Bool
esPrim n | n > 0 = [d | d <- [1..n], n `mod` d == 0] == [1,n]
         | otherwise = False

getPrimos :: Int -> [Int]
getPrimos n = filter esPrim [1..n]
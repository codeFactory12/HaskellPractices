module CardValidation where

    toDigits :: Int -> [Int]
    toDigits n | n <= 0 = []
               | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

    toDigitsRev :: Int -> [Int]
    toDigitsRev n | n <= 0 = []
               | otherwise = reverse (toDigits n)

    doubleEveryOther :: [Int] -> [Int]
    doubleEveryOther [] = []
    doubleEveryOther [x] = [x]
    doubleEveryOther xs = reverse $ zipWith (*) (reverse xs) (cycle [1,2])

    sumDigits :: [Int] -> Int
    sumDigits []     = 0
    sumDigits (x:xs) = fromIntegral (sum (toDigits x)) + sumDigits xs

    validate :: Int -> Bool
    validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

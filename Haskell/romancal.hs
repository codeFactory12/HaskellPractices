module Main where
import GHC.Base (VecElem(Int16ElemRep))
import Text.XHtml (input)

-- "II + III" = "V"
-- "II - III" = "-I"
-- II * III = VI
-- VI : III = II
-- V % III = II

calRoman :: String -> String
calRoman input = result $ words input
        where 
            result :: [String] -> String
            result (x:y:z:xs) = case y of
                                "+" -> numberToRoman ( romanToNumber x + romanToNumber z ) 
                                "-" -> numberToRoman ( romanToNumber x - romanToNumber z )
                                "*" -> numberToRoman ( romanToNumber x * romanToNumber z )
                                ":" -> numberToRoman ( romanToNumber x `div` romanToNumber z )
                                "%"  -> numberToRoman ( romanToNumber x `mod` romanToNumber z )  
 


roman::[(String, Int)]
roman = [("I", 1), ("IV", 4), ("V", 5), ("IX", 9), ("X", 10), 
         ("XL", 40), ("L", 50), ("XC", 90), ("C", 100),
         ("CD", 400), ("D", 500), ("CM", 900), ("M", 1000) ]

roman2 = reverse roman

expr :: [(String, (Int -> Int -> Int))]
expr = [("+", (+)), ("-", (-)), ("*", (*)), (":", div), ("%", mod)]

-- II
romanToNumber :: String -> Int
            -- I:[I]
romanToNumber ""     = 0
-- romanToNumber (x:xs) | length xs > 0 && findNumber (x: head xs:[]) == [] =  
romanToNumber (x:xs) | length xs > 0 && getRoman (x: head xs:[]) =
                                getNumber (x: head xs:[]) + romanToNumber (tail xs)
                     | otherwise = getNumber [x] + romanToNumber xs
                     where 
                        getRoman :: String -> Bool
                        getRoman x = findNumber x /= []

                        findNumber :: String -> [(String, Int)]
                        findNumber x = [r |r <- roman, fst r == x]

                        getNumber :: String -> Int 
                        getNumber x = snd  $ head $ filter (\n -> fst n == x) roman

numberToRoman :: Int -> String
numberToRoman num = convertToRoman num 0
                 
convertToRoman :: Int -> Int ->  String
convertToRoman 0 _ = "" 
convertToRoman num pos | num >= snd (roman2 !! pos) = fst (roman2 !! pos) ++ convertToRoman (num - snd(roman2 !! pos)) pos  
                       | otherwise = convertToRoman num (pos+1)

main :: IO()
main = do strings <- getContents
          mapM_ putStrLn $ map calRoman $ lines strings



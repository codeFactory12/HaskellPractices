{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE BlockArguments #-}
module Main where
    import GHC.Base (VecElem(Int16ElemRep))

    roman :: [(String, Int)]
    roman = [("I",1),("IV",4),("V",5),("IX",9),("X",10),("XL",40),("L",50),("XC",90),("C",100),("CD",400),("D",500),("CM",900),("M",1000)]

    sroman :: [(String, Int)]
    sroman = [("I",1),("V",5),("X",10),("L",50),("C",100),("D",500),("M",1000)]

    droman :: [(String, Int)]
    droman = [("IV",4),("IX",9),("XL",40),("XC",90),("CD",400),("CM",900)]


    romanToNumber :: String -> Int
    romanToNumber [] = 0
    romanToNumber [x] = romanCharToInt [x]
    romanToNumber (x:y:xs)
        | ([x,y] `elem` (map fst droman)) = romanDToInt [x,y] + romanToNumber xs
        | otherwise = romanCharToInt [x] + romanToNumber (y:xs)

    romanCharToInt :: String -> Int
    romanCharToInt x = case lookup x sroman of
        Just value -> value
        Nothing    -> error $ "Carácter romano no válido: " ++ x

    romanDToInt :: String -> Int
    romanDToInt x = case lookup x droman of
        Just value -> value
        Nothing    -> error "Combinación de caracteres romanos no válida"

    divString :: Char -> String -> [String]
    divString _ [] = []
    divString d xs = case break (== d) xs of
        (pr, _ :sf) -> pr : divString d sf
        (prefix, [])         -> [prefix]

    romanSum :: String -> String -> Int
    romanSum num1 num2 = romanToNumber num1 + romanToNumber num2

    romanSub :: String -> String -> Int
    romanSub num1 num2 = romanToNumber num1 - romanToNumber num2

    romanMult:: String -> String -> Int
    romanMult num1 num2 = romanToNumber num1 * romanToNumber num2

    romanDiv:: String -> String -> Int
    romanDiv num1 num2 = romanToNumber num1 `div` romanToNumber num2

    sdigit :: [(Int, String)]
    sdigit = [(1,"I"),(5,"V"),(10,"X"),(50,"L"),(100,"C"),(500,"D"),(1000,"M")]

    ddigit :: [(Int, String)]
    ddigit = [(4,"IV"),(9,"IX"),(40,"XL"),(90,"XC"),(400,"CD"),(900,"CM")]

    numberToRoman :: Int -> String
    numberToRoman 0 = ""
    numberToRoman num = getRoman num
            where
                getRoman :: Int -> String
                getRoman x = fst $ head $ filter (\n -> snd n == x) roman

    getRoman :: Int -> Int -> [String]
    getRoman num ctr
        | num >= roman = makeRoman(roman) ++ getRoman(num - roman) ctr
        | num < roman && num > 0 = getRoman(num) (ctr+1)
        | num <= 0 = []
            where roman = romanValues [] !! ctr

    makeRoman :: Int -> [String]
    makeRoman(1) = ["I"]; makeRoman(4) = ["IV"]; makeRoman(5) = ["V"];
    makeRoman(9) = ["IX"]; makeRoman(10) = ["X"]; makeRoman(40) = ["XL"];
    makeRoman(50) = ["L"]; makeRoman(90) = ["XC"]; makeRoman(100) = ["C"];
    makeRoman(400) = ["CD"]; makeRoman(500) = ["D"]; makeRoman(900) = ["CM"];
    makeRoman(1000) = ["M"]

    romanValues :: [Int] -> [Int]
    romanValues val = [1000,900,500,400,100,90,50,40,10,9,5,4,1]
    
    showRomans :: Int -> String
    showRomans num = concat $ getRoman num 0

    expr :: [(Char, Int -> Int -> Int)]
    expr = [('+',(+)),('-',(-)),('*',(*)),('/',div),('%',mod)]

    makeExpr :: Char -> Maybe (Int -> Int -> Int)
    makeExpr c = lookup c expr

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


    --main :: IO()
    ---main = do strings <- getContents
              --mapM_ putStrLn $ map calRoman $ lines strings

    main :: IO ()
    main = mapM_ putStrLn . map calRoman . lines =<< getContents



-- map f H [A,B,C,A] = [H,H,H,H]
-- filter f A [A,B,C,A] = [A,A]
-- foldl f Z [A,B,C,A] = Z


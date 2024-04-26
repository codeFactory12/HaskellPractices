module SuperDigit where

superDigit :: String -> Int
superDigit  []  = 0
superDigit  all@(x:xs) = if result > 9
                        then superDigit (show result)
                        else result
                        where result = read [x] + superDigit xs

multiCad :: String -> Int -> String
multiCad xs 1 = xs
multiCad xs n = xs ++ multiCad xs (n-1)

process :: String -> Int
process [] = 0
process xs =
    let palabras = words xs
    in superDigit (multiCad (head palabras) (read (last palabras)))

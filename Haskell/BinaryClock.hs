module BinClock where

splitChunks :: Int -> String -> [String]
splitChunks _ [] = []
splitChunks n xs = take n xs : splitChunks n (drop n xs)

binaryToDecimal :: String -> Int
binaryToDecimal = foldl (\acc x -> acc * 2 + read [x]) 0

validateTime :: [String] -> String
validateTime clock@[hours, minutes, seconds, _]
    | h < 24 && m < 60 && s < 60 = formatTime h m s
    | otherwise = "ERROR"
    where
        [h, m, s] = map binaryToDecimal clock

        formatTime :: Int -> Int -> Int -> String
        formatTime h m s = pad h ++ ":" ++ pad m ++ ":" ++ pad s
            where pad n = if n < 10 then '0' : show n else show n


main :: IO ()
main = do
    t <- readLn :: IO Int
    _ <- getLine 
    times <- sequence $ replicate t $ do
        clock <- sequence $ replicate 4 getLine
        _ <- getLine
        return clock
    let result = map validateTime times
    mapM_ putStrLn result

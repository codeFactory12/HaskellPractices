module StringOPermute where
swapChars :: String -> String
swapChars str = let n = length str
                    swapped = zipWith (\i c -> if i `mod` 2 == 0 then str !! (i+1) else str !! (i-1)) [0..] str
                in swapped

main :: IO ()
main = do
    t <- readLn :: IO Int
    testCases <- sequence (replicate t getLine)
    let results = map swapChars testCases
    mapM_ putStrLn results

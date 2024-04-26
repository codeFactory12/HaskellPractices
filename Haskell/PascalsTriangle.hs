module PascalsTriangle where
pascal :: Int -> [[Int]]
pascal n = take n (iterate nextRow [1])

nextRow row = zipWith (+) (0 : row) (row ++ [0])

main :: IO ()
main = do
    n <- readLn :: IO Int
    let triangle = pascal n
    mapM_ (\row -> putStrLn (unwords (map show row))) triangle


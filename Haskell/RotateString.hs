module RotateString where

import Control.Monad (replicateM_)

rotateLeft :: String -> String
rotateLeft [] = []
rotateLeft (x:xs) = xs ++ [x]

rotations :: String -> [String]
rotations str = take (length str) (iterate rotateLeft str)

main :: IO ()
main = do
    t <- readLn :: IO Int
    replicateM_ t $ do
        str <- getLine
        let rotatedStrs = rotations str
        putStrLn $ unwords rotatedStrs

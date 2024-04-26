module ListReplication where

import Control.Monad (replicateM)

listReplication ::  [Int] -> Int -> [Int]
listReplication [] 0 = []
listReplication  xs n = concatMap (replicate n) xs

main :: IO ()
main = do
    numbers <- fmap (map read . lines) getContents :: IO [Int]
    let (number:rest) = numbers
    print number
    let res = listReplication rest number
    print res



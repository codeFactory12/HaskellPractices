module UpdateList where
    
absoluto :: Int -> Int
absoluto n = if n < 0 then -n else n

main :: IO ()
main = do
    inputs <- getContents
    mapM_ (\x -> print (absoluto (read x))) (lines inputs)


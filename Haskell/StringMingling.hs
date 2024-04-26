module StringMingling where


stringMing :: String -> String -> String
stringMing [] [] = []
stringMing (x:xs) (y:xy) = x : y : stringMing xs xy
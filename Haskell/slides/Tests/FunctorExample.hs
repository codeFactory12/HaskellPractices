module FunctorExample where

double :: [Int] -> [Int]
double = fmap (*2)

duplicate :: String -> String
duplicate = fmap replaceSpace

replaceSpace :: Char -> Char
replaceSpace x = ' '

addComents :: [String] -> [String]
addComents =  fmap coment

coment :: String -> String
coment str = "<div>" ++ str ++ "</div>"

pTitleSlide = "hola como estas" <$ " "

data Elem = Dolar Int
        | Coin Int
        deriving Show

replacer :: [Elem] -> [Elem]
replacer str = Coin 10 <$ filter isDolar str

isDolar :: Elem -> Bool
isDolar (Dolar _) = True
isDolar _ = False

e0 = [Dolar 1, Coin 2,Coin 10, Dolar 1]
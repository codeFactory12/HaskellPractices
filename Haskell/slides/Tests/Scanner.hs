module Scanner where
import Data.Char (isSpace, isAlphaNum)
-- cabal install uulib

type Col = Int
type Line = Int
type Value = String
type Input = String

data Token = Token Type Value Line Col
    deriving(Eq)

data Type = String
          | OpenBlock
          | EndBlock
          | Keyword
          | EndSlide
          | Error
          | Comment
        deriving(Eq)

instance Show Token where
    show (Token t v l c) = show t ++ show v ++ " " ++ show l ++ " " ++ show c ++ "\n"

instance Show Type where
    show String = "String: "
    show OpenBlock = "OpenBlock: "
    show EndBlock = "EndBlock: "
    show Keyword = "Keyword: "
    show Error = "Error: "
    show EndSlide = "EndSlide: "
    show Comment = "Comment: "


scanner :: Input -> [Token]
scanner xs = scan xs 1 1

scan :: Input -> Line -> Col -> [Token]
scan [] _ _ = []
scan (x:xs) l c
    -- lector de titulos
  | x == '!' = if not (null xs) && head xs == ' ' then Token Keyword [x, head xs] l c: scan xs l (c+1) else scan xs l (c+ 1)
  | x == '-' = if scanSlideEnd xs then Token EndSlide ['-','-','-'] l c : scan xs l (c+2) else scan xs l (c + 1)
  | x == '{' = if head xs == '\n' then Token OpenBlock [x] l c : scan xs l (c+1) else scan xs l (c+1)
  | x == '}' = if head xs == '\n' then Token EndBlock [x] l c : scan xs l (c+1) else scan xs l (c+1)
  | x == '#' = let (hashes, rest) = span (== '#') xs
                in if length (x:hashes) <= 6 then Token Keyword (x:hashes) l c : scan rest l (c + length (x:hashes)) else scan (dropWhile (/= '\n') xs) l 1
  | isAlphaNum x =  let (alphaList, rest) = span (\c -> isAlphaNum c ||  c == ' ') xs
                   in Token String (x : alphaList) l c : scan rest l (c + length (x : alphaList))
  | x == ';' = scan (dropWhile (/= '\n') xs) l 1
  | x == '\n' || x == ';' = scan xs (l+1) 1
  | otherwise = scan xs l (c+1)

scanSlideEnd :: Input -> Bool
scanSlideEnd [] = False
-- ### es subtitulo
-- ; es comentario debe saltarse la linea
scanSlideEnd (x:xs)
            | x == '-' = (not (null xs) && head xs == '-') || False
            | otherwise = False
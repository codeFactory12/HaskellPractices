{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module ExpressionV2 where

import Data.Char (isSpace)
modulo = 10^9 +7

--Expresiones donde const es el numero
data Exp = Sum Term Exp
         | Sus Term Exp
         | JustTerm Term
         deriving (Eq, Show)

data Term = Mult Factor Term
         |Div Factor Term
         |JustFactor Factor
         deriving (Eq, Show)

data Factor  = Num Int
         |Paren Exp
         |JustExp Exp
         deriving(Eq, Show)

-- Evaluar expresiones
evalExp :: Exp -> Int
evalExp (JustTerm t) = evalTerm t
evalExp (Sum a b) = (evalTerm a + evalExp b) `mod` modulo
evalExp (Sus a b) = (evalTerm a - evalExp b) `mod` modulo

evalFactor :: Factor -> Int
evalFactor (JustExp e) = evalExp e
evalFactor (Num n) = n
evalFactor (Paren e) = evalExp e

evalTerm :: Term -> Int
evalTerm (JustFactor f) = evalFactor f
evalTerm (Mult a b) = (evalFactor a * evalTerm b) `mod` modulo

-- evalTerm (Div a b) = (evalFactor a `div` evalTerm b) `mod` modulo
evalTerm (Div a b) = 
    let a' = evalFactor a
        b' = evalTerm b
    in (a' * modInv b') `mod` modulo

-- Calcular el inverso multiplicativo modular
-- 1
modInv :: Int -> Int
modInv b =
        let (_, x, _) = mcdCoef b modulo
        in x `mod` modulo
        
-- interpretar modulo de la division
mcdCoef :: Int -> Int -> (Int, Int, Int)
mcdCoef a 0 = (a,1,0)
mcdCoef a b =
        let (d, x, y) = mcdCoef b (a `mod` b)
        in (d, y, x - (a `div` b) * y)

etest = Sum (JustFactor (Num 2)) (Sum (JustFactor (Num 3)) (JustTerm (JustFactor(Num 4))))

e0 = Sus ( Mult (Num 22) (JustFactor (Num 79))) (JustTerm (JustFactor (Num 21)))

e1 = Sum (JustFactor (Num 1)) (JustTerm (JustFactor (Num 2)))

e2 = Div(Num 1) (JustFactor (Num 2))

-- Función para analizar una expresión
parseExp :: String -> (Exp, String)
parseExp str =
    let (term1, rest1) = parseTerm str
    in case rest1 of
        ('+':rest2) ->
            let (term2, rest3) = parseExp rest2
            in (Sum term1 term2, rest3)
        ('-':rest2) ->
            let (term2, rest3) = parseExp rest2
            in (Sus term1 term2, rest3)
        _ -> (JustTerm term1, rest1)

-- Función para analizar un término
parseTerm :: String -> (Term, String)
parseTerm str =
    let (factor1, rest1) = parseFactor str
    in case rest1 of
        ('*':rest2) ->
            let (factor2, rest3) = parseTerm rest2
            in (Mult factor1 factor2, rest3)
        ('/':rest2) ->
            let (factor2, rest3) = parseTerm rest2
            in (Div factor1 factor2, rest3)
        _ -> (JustFactor factor1, rest1)

-- Función para analizar un factor
parseFactor :: String -> (Factor, String)
parseFactor ('(':str) =
    let (exp, rest) = parseExp str
    in case rest of
        (')':rest') -> ( Paren exp, rest')
-- "/-" "negative numbers"
parseFactor ('-':str) =
    let (num, rest) = span (`elem` ['0'..'9']) str
    in (Num (read ('-' : num)), rest)
parseFactor str =
    let (num, rest) = span (`elem` ['0'..'9']) str
    in (Num (read num), rest)


-- Remover espacios en blanco
removeEmptySpaces :: String -> String
removeEmptySpaces = filter (not . isSpace)

-- Función para analizar una cadena en una expresión
parseExpression :: String -> Exp
parseExpression str =
    let (exp, rest) = parseExp (removeEmptySpaces str)
    in if null rest
        then exp
        else error "Error de sintaxis: entrada no completamente analizada"

parseRes :: String -> Int
parseRes str = evalExp (parseExpression str)

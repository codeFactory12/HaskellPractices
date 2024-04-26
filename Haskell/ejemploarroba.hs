data Punto = Punto { x :: Double, y :: Double }

instance Show Punto where
    show (Punto xCoord yCoord) = "(" ++ show xCoord ++ ", " ++ show yCoord ++ ")"

-- Función para calcular la distancia entre dos puntos
distanciaEntrePuntos :: Punto -> Punto -> Double
distanciaEntrePuntos punto1@(Punto x1 y1) punto2@(Punto x2 y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

distanciaEntrePuntos' :: Punto -> Punto -> Double
distanciaEntrePuntos' (Punto x1 y1) (Punto x2 y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

capital :: String -> String
capital "" = "¡Una cadena vacía!"
capital all@(x:_) = "La primera letra de " ++ all ++ " es " ++ [x]

capital' :: String -> String
capital' "" = "¡Una cadena vacía!"
capital' (x:xs) = "La primera letra de " ++ (x:xs) ++ " es " ++ [x]


main :: IO ()
main = do
    let punto1 = Punto { x = 1.0, y = 1.0 }
        punto2 = Punto { x = 4.0, y = 5.0 }
        distancia = distanciaEntrePuntos punto1 punto2
    putStrLn $ "Punto 1: " ++ show punto1
    putStrLn $ "Punto 2: " ++ show punto2
    putStrLn $ "Distancia entre los puntos: " ++ show distancia


readNInput :: IO [String]
readNInput = do
    nLinesStr <- getLine
    let nLines = read nLinesStr :: Int
    sequence $ replicate nLines getLine


puntoPendiente :: (Double, Double) -> (Double, Double) -> Double
puntoPendiente (x1, y1) (x2, y2) = let angle = atan2 (y2 - y1) (x2 - x1) in angle * 180 / pi

parsePoints :: [String] -> [(Double, Double)]
parsePoints = map (\[x, y] -> (read x, read y)) . map words

main :: IO ()
main = do

    input <- readNInput
    let numPoints = read (head input) :: Int
        points = parsePoints (tail input)

    let pendientes = [puntoPendiente p1 p2 | (p1, p2) <- zip points (tail points)]
    
    let algunoMayor180 = any (> 180) pendientes
    
    if algunoMayor180
        then putStrLn "SÍ"
        else putStrLn "NO"

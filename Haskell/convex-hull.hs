module ConvexHull where
import Control.Monad (replicateM)
import Data.Foldable (minimumBy)
import Data.List

type Point = (Double, Double)

-- Calculate perimeter
distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2


perimeter :: [Point] -> Double
perimeter hull =
    let cyclicHull = hull ++ [head hull]
    in sum $ zipWith distance cyclicHull (tail cyclicHull)
    
parsePoint :: String -> Point
parsePoint str =
    let [x, y] = map read $ words str
    in (x, y)

point1 :: Point
point1 = (1.0, 1.0)

point2 :: Point
point2 = (2.0, 5.0)

point3 :: Point
point3 = (3.0, 3.0)

point4 :: Point
point4 = (5.0, 3.0)

point5 :: Point
point5 = (3.0, 2.0)

point6 :: Point
point6 = (2.0, 2.0)

-- Convex-Hull jarvis March
{--
polarAngle :: Point -> Point -> Double
polarAngle (x1, y1) (x2, y2) = atan2 (y2 - y1) (x2 - x1)

leftmostPoint :: [Point] -> Point
leftmostPoint = minimumBy (\(x1, _) (x2, _) -> compare x1 x2)

jarvisMarch :: [Point] -> [Point]
jarvisMarch [] = []
jarvisMarch points =
    let leftpoint = leftmostPoint points
        nextPoint hull point =
            if point == leftpoint
                then hull
                else case hull of
                    [] -> [point]
                    [p] -> [p, point]
                    _ -> if polarAngle (last hull) point < polarAngle (last hull) (head hull)
                            then hull ++ [point]
                            else hull
        finalHull = reverse $ go [leftpoint] (filter (/= leftpoint) points)
          where
            go hull [] = hull
            go hull (p:ps) = go (nextPoint hull p) ps
    in finalHull
    --}
dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1, y1) (x2, y2) = sqrt (f x1 x2 + f y1 y2)
  where f a b = (a - b)^2

ccv :: Num a => (a, a) -> (a, a) -> (a, a) -> a
ccv (x1, y1) (x2, y2) (x3, y3) = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)

findMinYPoint :: [Point] -> Point
findMinYPoint (p:points) = f p points
  where
    f prev [] = prev
    f p0@(x0,y0) (p1@(x1,y1):points) | y1 < y0 = f p1 points
                                     | (y1 == y0) && (x1 < x0) = f p1 points
                                     | otherwise = f p0 points

filterSameAngle :: [(Point, Double)] -> [Point]
filterSameAngle lst = map fst . filter snd $ r
  where
    r = zipWith (\(pa, ra) (pb, rb) -> (pa, ra /= rb)) lst ((drop 1 lst) ++ [head lst])

build :: [Point] -> [Point] -> [Point]
build hull [] = hull
build hs@(h1:[]) ps@(p:points) = build (p:h1:[]) points
build hs@(h2:h1:hull) ps@(p:points) = hull'
  where
    rightTurn = ccv h1 h2 p < 0
    collinear = ccv h2 h1 p == 0
    hull' | rightTurn = build (h1:hull) ps  
          | collinear = build (p:h1:hull) points  
          | otherwise = build (p:hs) points  

convexHull :: [(Double, Double)] -> [(Double, Double)]
convexHull points = hull
  where
    -- explicar el "@"
    p0@(x0, y0) = findMinYPoint points

    sorted' = let
                 o (a, ra) (b, rb) | ra > rb = GT
                                   | (ra == rb) && ((dist a p0) > (dist b p0)) = GT
                                   | otherwise = LT
             in sortBy o hullP

    f (x, y) = r'
      where r = atan $ (y - y0) / (x - x0)
            r' | r < 0 = r + 2*pi
               | otherwise = r

    hullP = map (\p -> (p, f p)) $ delete p0 points

    sorted = filterSameAngle sorted'

    hull = build [p0] sorted


readPoints :: Int -> IO [Point]
readPoints 0 = return []
readPoints n = do
    pointStr <- getLine
    let point = parsePoint pointStr
    points <- readPoints (n - 1)
    return (point : points)

main :: IO ()
main = do
    n <- readLn :: IO Int
    points <- readPoints n
    let convexHully = convexHull points
        result = perimeter convexHully
    print result
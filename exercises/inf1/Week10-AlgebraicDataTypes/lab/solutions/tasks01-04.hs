main :: IO()
main = do
    print $ Circle 5
    print $ Triangle 3 4 5
    print $ Rectangle 2 42
    print $ Cylinder 5 3.14
    print $ perimeter (Circle 5) == 31.41592653589793
    print $ perimeter (Rectangle 2.5 4.5) == 14
    print $ perimeter (Rectangle 5.5 20.6) == 52.2
    print $ perimeter (Triangle 5.3 3.9 4.89) == 14.09
    print $ perimeter (Cylinder 2.5 10) == 30

    print $ area (Circle 5) == 78.53981633974483
    print $ area (Rectangle 2.5 4.5) == 11.25
    print $ area (Rectangle 5.5 20.6) == 113.30000000000001
    print $ area (Triangle 5.3 3.9 4.89) == 9.127927385194024
    print $ area (Cylinder 20 30) == 6283.185307179587  

    print $ isRound (Circle 5) == True
    print $ isRound (Rectangle 2.5 4.5) == False
    print $ isRound (Rectangle 5.5 20.6) == False
    print $ isRound (Triangle 5.3 3.9 4.89) == False
    print $ isRound (Cylinder 20 30) == True

    print $ is2D (Circle 5) == True
    print $ is2D (Rectangle 2.5 4.5) == True
    print $ is2D (Rectangle 5.5 20.6) == True
    print $ is2D (Triangle 5.3 3.9 4.89) == True
    print $ is2D (Cylinder 20 30) == False

    print $ getAreas [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == [78.54, 11.25, 113.3, 9.13, 6283.19]

    print $ maxArea [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == Cylinder 20.0 30.0

data Shape a = Circle a | Triangle a a a | Rectangle a a | Cylinder a a
    deriving (Show, Eq)

perimeter :: (Floating a) => Shape a -> a
perimeter (Circle r) = 2 * pi * r
perimeter (Triangle x y z) = x + y + z
perimeter (Rectangle x y) = 2 * (x + y)
perimeter (Cylinder r h) = 4 * r + 2 * h

area :: (Floating a) => Shape a -> a
area (Circle r) = pi * r * r
area t@(Triangle x y z) = let p = perimeter t / 2 in sqrt $ p * (p - x) * (p - y) * (p - z)
area (Rectangle x y) = x * y
area (Cylinder r h) = 2 * pi * r * h + 2 * pi * r * r

isRound :: Shape a -> Bool
isRound (Circle _) = True
isRound (Cylinder _ _) = True
isRound _ = False

is2D :: Shape a -> Bool
is2D (Cylinder _ _) = False
is2D _ = True

roundTwoDig :: (Floating a, RealFrac a) => a -> a
roundTwoDig = (/ 100) . fromIntegral . round .(* 100)

getAreas :: (Floating a, RealFrac a) => [Shape a] -> [a]
-- getAreas [] = []
-- getAreas (x:xs) = roundTwoDig (area x) : getAreas xs
getAreas = map (roundTwoDig . area)

maxArea :: (Floating a, RealFrac a) => [Shape a] -> Shape a
maxArea (x:xs) = foldl (\ acc s -> if area s > area acc then s else acc) x xs
main :: IO()
main = do
    print $ sumVectors (1, 2, 3) (4, 5, 6) == (5, 7, 9)
    print $ sumVectors (0, 0, 0) (100, 200, -300) == (100, 200, -300)

    print $ scaleBy (1, 2, 3) 5 == (5, 10, 15)
    print $ scaleBy (5, 2, 159) (-2) == (-10, -4, -318)

    print $ dotProduct (1, 2, 3) (7, 4, 1) == 18
    print $ dotProduct (5, 2, 159) (0, -1, -2) == -320

    print $ magnitude (1, 2, 3) == 3.7416573867739413
    print $ magnitude (7, 4, 1) == 8.12403840463596
    print $ magnitude (-10, 20, -10) == 24.49489742783178
    print $ magnitude (5, 2, 159) == 159.0911688309568
    print $ magnitude (0, -1, -2) == 2.23606797749979
    print $ magnitude (155, 10, -5) == 155.40270267920053

type Vector a = (a, a, a)

sumVectors :: (Num a) => Vector a -> Vector a -> Vector a
sumVectors (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

scaleBy :: (Num a) => Vector a -> a -> Vector a
scaleBy (x, y, z) n = (x * n, y * n, z * n)

dotProduct :: (Num a) => Vector a -> Vector a -> a
dotProduct (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

magnitude :: (Num a, Floating a) => Vector a -> a
magnitude x = sqrt $ dotProduct x x
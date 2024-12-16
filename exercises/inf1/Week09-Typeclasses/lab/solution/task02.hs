main :: IO()
main = do
    print $ toTuples [1, 2, 3, 4] [4, 3, 2, 1] == [(1, 4), (2, 3), (3, 2), (4, 1)]
    print $ toTuples [1.52, 2.75] [] == [(1.52, 0), (2.75, 0)]

toTuples :: (Num a) => [a] -> [a] -> [(a, a)]
toTuples [] [] = []
toTuples (x:xs) [] = (x, 0) : toTuples xs []
toTuples [] (y:ys) = (0, y) : toTuples [] ys
toTuples (x:xs) (y:ys) = (x, y) : toTuples xs ys

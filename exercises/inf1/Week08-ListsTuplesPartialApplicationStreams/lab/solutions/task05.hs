main :: IO()
main = do
    print $ pack [1, 2, 3, 7, 8 ,9] == [[1,2,3],[7,8,9]]
    print $ pack [1, 7, 8 ,9] == [[1],[7,8,9]]
    print $ pack [1, 9] == [[1],[9]]
    print $ pack [1, 7, 8 ,10] == [[1],[7,8],[10]]

pack :: [Int] -> [[Int]]
pack [] = []
pack (x:xs) = map reverse $ helper [x] xs
 where
    helper :: [Int] -> [Int] -> [[Int]]
    helper res [] = [res]
    helper (r:rs) (y:ys)
     | y == r + 1 = helper (y:r:rs) ys
     | otherwise = (r:rs) : helper [y] ys

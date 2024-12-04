main :: IO()
main = do
    print $ addOneXs [1, 2, 3, 4, 5] == [2, 3, 4, 5, 6]
    print $ addOneN 5 == 6
    print $ sqPlusOne 5 == 26

addOneXs :: [Int] -> [Int]
addOneXs = map (+ 1)

addOneN :: Int -> Int
addOneN = (1 +)

sqPlusOne :: Int -> Int
sqPlusOne = (+ 1) . (^ 2)
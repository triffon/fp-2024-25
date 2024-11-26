main :: IO()
main = do
    print $ (take 8 $ myCycle [1, 2, 3]) == [1, 2, 3, 1, 2, 3, 1, 2]

myCycle :: [a] -> [a]
myCycle xs = xs ++ myCycle xs
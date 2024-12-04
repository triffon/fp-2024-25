main :: IO()
main = do
    print $ areAmicable 200 300 == False
    print $ areAmicable 220 284 == True
    print $ areAmicable 284 220 == True
    print $ areAmicable 1184 1210 == True
    print $ areAmicable 2620 2924 == True
    print $ areAmicable 6232 6368 == True

getSumOfDivisors :: Int -> Int
areAmicable :: Int -> Int -> Bool

getSumOfDivisors n = sum (filter (\ x -> mod n x == 0) [1 .. div n 2])

areAmicable x y = getSumOfDivisors x == y || x == getSumOfDivisors y
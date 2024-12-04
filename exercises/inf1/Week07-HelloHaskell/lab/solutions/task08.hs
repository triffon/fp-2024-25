main :: IO()
main = do
    print $ isPrime 1 == False
    print $ isPrime 2 == True
    print $ isPrime 3 == True
    print $ isPrime 6 == False
    print $ isPrime 61 == True

intSqrt :: Int -> Int
isPrime :: Int -> Bool

intSqrt = floor . sqrt . fromIntegral

isPrime n = n /= 1 && length [ x | x <- [2 .. intSqrt n], mod n x == 0] == 0
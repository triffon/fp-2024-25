main :: IO()
main = do
    print $ isPerfect 1 == False
    print $ isPerfect 6 == True
    print $ isPerfect 495 == False
    print $ isPerfect 33550336 == True

isPerfect :: Int -> Bool
isPerfect n = n == sum [x | x <- [1 .. div n 2], mod n x == 0]
main :: IO()
main = do
    print $ dividePM (10, 5) == (2, 0)
    print $ dividePM (69, 42) == (1, 27)

    print $ (\ (x, y) -> ((x `div` y, x `mod` y))) (10, 5) == (2, 0)
    print $ (\ (x, y) -> ((x `div` y, x `mod` y))) (69, 42) == (1, 27)

    print $ stoyanF (1, 2) 3 == 6
    print $ ((\ (x, y) z -> x + y + z) (1, 2) 3) == 6

dividePM :: (Integral a) => (a, a) -> (a, a)
dividePM (_, 0) = error "Division by 0"
dividePM (x, y) = (x `div` y, x `mod` y)

stoyanF :: (Int, Int) -> Int -> Int
stoyanF (x, y) z = x + y + z
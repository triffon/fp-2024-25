main :: IO()
main = do
    print $ [1 .. 5]
    print $ [1, 3 .. 6]
    print $ [x + 10 | x <- [1 .. 6]]
    print $ [x | x <- [1 .. 6], even x]
    print $ [x | x <- [1 .. 6], mod x 3 == 0 && x > 3]
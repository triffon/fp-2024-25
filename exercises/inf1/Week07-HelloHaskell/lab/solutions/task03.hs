main :: IO()
main = do
    print $ fibRec 11 == 89

    print $ fibIter 11 == 89
    print $ fibIter 110 == 43566776258854844738105

fibRec :: Int -> Int
fibIter :: Int -> Integer

fibRec 0 = 0
fibRec 1 = 1
fibRec n = fibRec (n - 1) + fibRec (n - 2)

fibIter n = helper 0 1 n
 where
    helper :: Integer -> Integer -> Int -> Integer
    helper n0 _ 0 = n0
    helper _ n1 1 = n1
    helper n0 n1 current = helper n1 (n0 + n1) (current - 1)
import Data.List

empty :: [Int]
empty = []

xs1 :: [Int]
xs1 = [1, 2, 3, 4]

xs2 :: [Int]
xs2 = 1 : []

xs3 :: [Int]
xs3 = 1 : [2, 3]

isEmpty :: [Int] -> Bool
isEmpty xs = null xs 

car :: [Int] -> Int -- head
car [] = error "Empty list"
car (x:xs) = x

cdr :: [Int] -> [Int] -- tail
cdr [] = error "Empty list"
cdr (x:xs) = xs

at :: [Int] -> Int -> Int
at xs i = xs!!i

-- last

main :: IO()
main = do
    print $ elem 1 [1, 2, 3, 4]
    print $ elem 1 [5, 2, 3, 4]
    print $ [1, 2, 3] ++ [4, 5, 6]
    print $ length [1, 2, 3]
    print $ reverse [1, 2, 3]
    print $ sum [1, 2, 3]
    print $ product [1, 2, 3]
    print $ minimum [1, 2, 3] -- maximum
    print $ take 2 [1, 2, 3]
    print $ take 5 [1, 2, 3]
    print $ drop 2 [1, 2, 3]
    print $ drop 5 [1, 2, 3]
    print $ nub [1, 2, 1, 2, 3, 3, 1, 1]
    print $ sort [5, 3, 4, 1, 2]
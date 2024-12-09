-- 1 2  3 4  5 6
-- 0 1 -1 2 -2 3

-- is odd -> (1 - n) / 2
-- is even -> n / 2

main :: IO()
main = do
    print $ (take 10 ints) == [0,1,-1,2,-2,3,-3,4,-4,5]

nats :: [Int]
nats = [1 ..]

natToInt :: Int -> Int
natToInt x = if odd x then div (1 - x) 2 else div x 2

ints :: [Int]
ints = [ y | x <- nats, y <- [natToInt x]]


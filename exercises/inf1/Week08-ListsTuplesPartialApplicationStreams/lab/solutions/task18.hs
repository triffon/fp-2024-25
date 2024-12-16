main :: IO()
main = do
    print $ (take 10 intPairs) == [(0,0), (0,1), (1,0), (0,-1), (1,1), (-1,0), (0,2), (1,-1), (-1,1), (2,0)]

nats :: [Int]
nats = [1 ..]

natPairs :: [(Int, Int)]
-- natPairs = [(x, y) | s <- nats, x <- [1 .. s - 1], y <- [1 .. s - 1], x + y == s]
natPairs = [ (x, y) | z <- nats, x <- [1 .. z], y <- [z - x + 1]]

natToInt :: Int -> Int
natToInt x = if odd x then div (1 - x) 2 else div x 2

intPairs :: [(Int, Int)]
intPairs = [ (natToInt x, natToInt y) | (x, y) <- natPairs]
main :: IO()
main = do
    print $ take 10 natPairs == [(1,1), (1,2), (2,1), (1,3), (2,2), (3,1), (1,4), (2,3), (3,2), (4,1)]

nats :: [Int]
nats = [1 ..]

natPairs :: [(Int, Int)]
natPairs = [(x, y) | s <- nats, x <- [1 .. s], y <- [1 .. s], x + y == s]
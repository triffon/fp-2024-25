nats :: [Int]
nats = [1 ..]

pairs :: [(Int, Int)]
pairs = [(x, y) | x <- nats, y <- nats]

coolPairs :: [(Int, Int)]
coolPairs = [(x, y) | x <- nats, y <- nats, odd x && even y]
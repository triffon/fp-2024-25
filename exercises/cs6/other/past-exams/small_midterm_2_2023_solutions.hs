data Tree = Empty | Node Int [Tree]
    deriving (Show)

prune :: Tree -> Tree
prune Empty = Empty
prune (Node v children) = Node v [t | t<-children, pred v t]
  where pred parent Empty = False
        pred parent (Node v _) = gcd parent v == 1

tree :: Tree
tree =
  Node
    10
    [ Node 3 [Empty, Node 3 [], Node 2 [Empty, Empty]],
      Node 7 [Node 14 [], Node 2 [], Node 1 [Empty], Node 15 [Empty]],
      Node 20 [Node 9 [Node 10 []]]
    ]

result :: Tree
result =
  Node
    10
    [ Node 3 [Node 2 []],
      Node 7 [Node 2 [], Node 1 [], Node 15 []]
    ]

mergeFromMaybes :: [Maybe a] -> [Maybe a] -> [a]
mergeFromMaybes xs ys = concatMap f $ zipWith (<|>) xs ys
  where Just x <|> _ = Just x
        _ <|> Just y = Just y
        Nothing <|> Nothing = Nothing
        f (Just x) = [x]
        f Nothing = []

toMaybes :: (a -> Bool) -> [a] -> [Maybe a]
toMaybes p = map (\x -> if p x then Just x else Nothing)

-- Example infinite strems with Maybes
s1 :: [Maybe Int]
s1 = toMaybes even [1..]

s2 :: [Maybe Int]
s2 = toMaybes odd [1..]

-- ... and so on (could use cycle as well to repeat some particular list [Maybe a])

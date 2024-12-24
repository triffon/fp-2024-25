main :: IO()
main = do
    print $ t
    print $ size t == 13
    print $ getLevel t 3 == [1, 2]

data NTree a = Nil | Node a [NTree a]
    deriving (Show, Eq)

t = Node 10 [Node 3 [Node 5 [Nil], Node 8 [Node 1 [Nil], Node 2 [Nil]], Node 9 [Nil]], Node 7 [Node 11 [Nil], Node 13 [Nil]], Node 12 [Node 6 [Nil], Node 4 [Nil]]]

size :: NTree a -> Int
size Nil = 0
size (Node _ children) = 1 + (sum $ map size children)

getLevel :: NTree a -> Int -> [a]
getLevel Nil _ = []
getLevel (Node x _) 0 = [x]
getLevel (Node _ children) k = concat $ map (\ child -> getLevel child (k - 1)) children
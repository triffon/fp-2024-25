data BinaryTree t =
  Empty |
  Node {rootTree :: t, leftTree :: BinaryTree t, rightTree :: BinaryTree t}
  deriving Show

rotateLeft :: BinaryTree t -> BinaryTree t
rotateLeft Empty = Empty
rotateLeft tree@(Node _ _ Empty) = tree
rotateLeft (Node b d (Node a e c)) = Node a (Node b d e) c

testTree::BinaryTree Integer
testTree = Node 5
                (Node 1
                      (Node 4
                            Empty
                            (Node 13 Empty Empty))
                      (Node 3 Empty Empty))
                (Node 8
                      (Node 0
                            (Node 10 Empty Empty)
                            (Node 9 Empty Empty))
                      (Node 11 Empty Empty))

bloom :: BinaryTree t -> BinaryTree t
bloom Empty = Empty
bloom leaf@(Node root Empty Empty) = Node root leaf leaf
bloom (Node root left right) = Node root (bloom left) (bloom right)

paths :: BinaryTree t -> [[t]]
paths Empty = [[]]
paths (Node root Empty Empty) = [[root]]
paths (Node root left right) = map (root:) $ paths left ++ paths right

newtype Graph t = Graph [(t, [t])]
  deriving Show

testGraph :: Graph Int
testGraph = Graph [(1, [2, 3, 4]),
                   (2, [1, 3, 5]),
                   (3, [1, 2, 4, 5]),
                   (4, [1, 3, 5]),
                   (5, [2, 3, 4])]

testOrientedGraph :: Graph Int
testOrientedGraph = Graph [(1, [2, 3, 4]),
                           (2, [3, 5]),
                           (3, [4, 5]),
                           (4, []),
                           (5, [2, 4])]

vertices :: Graph t -> [t]
vertices (Graph graph) = map fst graph

search :: Eq k => k -> [(k, v)] -> v -> v
search _ [] def = def
search key ((k, v):xs) def
  | key == k = v
  | otherwise = search key xs def

neighbours :: Eq t => t -> Graph t -> [t]
neighbours u (Graph graph) = search u graph []

edge :: Eq t => t -> t -> Graph t -> Bool
edge u v graph = v `elem` neighbours u graph

parents :: Eq t => t -> Graph t -> [t]
parents u (Graph graph) = vertices $ Graph $ filter ((u `elem`) . snd) graph

degree :: Eq t => Graph t -> [(Int, Int)]
degree graph = map (\u -> (length $ parents u graph, length $ neighbours u graph)) $ vertices graph

connectedComponent :: Eq t => t -> Graph t -> [t]
connectedComponent u graph = dfs u [u] graph
  where dfs :: Eq t => t -> [t] -> Graph t -> [t]
        dfs u visited graph = foldr (\v result -> if v `elem` result then result else dfs v (v:result) graph) visited $ neighbours u graph

pathsTo :: Eq t => t -> t -> Graph t -> [[t]]
pathsTo u v graph = pathsHelper u v graph []
  where pathsHelper :: Eq t => t -> t -> Graph t -> [t] -> [[t]]
        pathsHelper u v graph visited
          | u == v = [[u]]
          | u `elem` visited = []
          | otherwise = [u : path | neighbour <- neighbours u graph, path <- pathsHelper neighbour v graph (u : visited)]
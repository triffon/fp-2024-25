type Vertex = Int
-- Примерно представяне на граф - списък на съседство с Float тегла (за да не ги омешаме с някой индекс на връх)
type Graph = [(Vertex, [(Vertex, Float)])]

-- Имплемнтациите нямат значение - добавете ваша :)
neighbs :: Graph -> Vertex -> [(Vertex,Float)]
neighbs = undefined

-- Наивна приоритетна опашка - списък (връх,разстояние), сортиран по разстоянията
type PriorityQueue = [(Vertex, Float)]

-- Задача: променете функцията така, че ако дадения връх се среща в опашката,
-- да актуализира разстоянието до него вместо да добавя второ копие.
-- Бонус за който прочете това и пръв ми прати решение
-- Например:
--  insertInPQ [(1,2.5),(2,3.14)] (2,0.7) -> [(2,0.7),(1,2.5)]
--  insertInPQ [(1,2.5),(2,3.14)] (2,3.7) -> [(1,2.5),(2,3.14)]
insertInPQ :: PriorityQueue -> (Vertex, Float) -> PriorityQueue
insertInPQ pq (u,d) = a ++ ((u,d) : b)
  where a = takeWhile ((<d).snd) pq
        b = dropWhile ((<d).snd) pq

-- Намира дължината на най-късия път между дадени два върха
dijkstra :: Graph -> Vertex -> Vertex -> (Maybe Float)
dijkstra g start goal = helper [(start,0)] []
  where -- На една итерация изваждаме връх от приоритетната опашка, отбелязваме го като посетен,
        -- и добавяме непосетените му съседи в опашката (имаме нови кандидати за пътища към тях).
        helper :: PriorityQueue -> [Vertex] -> (Maybe Float)
        helper [] _ = Nothing
        helper ((u,dist):rest) visited
          | u == goal        = Just dist
          -- Може да минем без тази клауза, ако приоритетната опашка не позволява повторения на върхове
          | u `elem` visited = helper rest visited
          | otherwise        = helper pq' (u:visited)
          where pq' = foldl insertInPQ rest [ (v,dist+c) | (v,c)<-neighbs g u, not $ v `elem` visited ]

-- Намира дължините на най-късите пътища до от даден връх до всички достижими от него
dijkstraAll :: Graph -> Vertex -> [(Vertex,Float)]
dijkstraAll g start = helper [(start,0)] []
  where -- Няма нужда от изричен списък с посетени върхове - знаем, че за тях имаме сметнати вече разстояния
        helper :: PriorityQueue -> [(Vertex,Float)] -> [(Vertex,Float)]
        helper [] allDists = allDists
        helper ((u,dist):rest) allDists
          -- Може да минем без тази клауза, ако приоритетната опашка не позволява повторения на върхове
          | u `elem` visited = helper rest allDists
          | otherwise        = helper pq' ((u,dist):allDists)
          where visited = map fst allDists
                pq' = foldl insertInPQ rest [ (v,dist+c) | (v,c)<-neighbs g u, not $ v `elem` visited ]

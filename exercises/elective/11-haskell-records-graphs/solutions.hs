-- 1.
find :: (Eq a) => [(a, b)] -> a -> b
find [] key = error "Key not found"
find ((k, v) : xs) key
  | k == key = v
  | otherwise = find xs key


-- 2.
groupBy :: (Eq b) => (a -> b) -> [a] -> [(b, [a])]
groupBy _ [] = []
groupBy f (x : xs) = (key, same) : groupBy f rest
  where
    key = f x
    same = x : filter (\y -> f y == key) xs
    rest = filter (\y -> f y /= key) xs


-- 3.
type Subject = String   -- Име на предмет
type Student = String   -- Име на студент
type Exam = Int         -- Идентификатор на даден изпит
type Note = Double      -- Оценка: число от 2 до 6

-- Запис с оценката на даден студент за даден изпит по даден предмет
type Record = (Subject, Student, Exam, Note)

-- Помощна функция за пресмятане на средна стойност
average :: [Double] -> Double
average xs = sum xs / fromIntegral (length xs)

-- Помощна функция за максимум по дадена функция
maximumOn :: (Ord b) => (a -> b) -> [a] -> a
maximumOn f = foldl1 (\x y -> if f x > f y then x else y)

averageNote :: [Record] -> Double
averageNote recs = average [note | (_, _, _, note) <- recs]

-- Помощна функция за премахване на повтавящи се елементи от даден списък
removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : removeDuplicates (filter (/= x) xs)

-- Решение на задачата
topOfClass :: [Record] -> Student
topOfClass records = fst $ maximumOn snd studentAverages
  where
    studentRecords = groupBy (\(_, student, _, _) -> student) records
    studentAverages = [(student, averageNote recs) | (student, recs) <- studentRecords]

hardestExam :: [Record] -> Exam
hardestExam records = fst $ maximumOn (negate . snd) examAverages
  where
    examRecords = groupBy (\(_, _, exam, _) -> exam) records
    examAverages = [(exam, averageNote recs) | (exam, recs) <- examRecords]

perfectScorers :: [Record] -> Subject -> [Student]
perfectScorers records subject = [student | (student, averageNote) <- studentAverages, averageNote == 6.0]
  where
    subjectRecords = filter (\(s, _, _, _) -> s == subject) records
    studentRecords = groupBy (\(_, student, _, _) -> student) subjectRecords
    studentAverages = [(student, averageNote recs) | (student, recs) <- studentRecords]

failingStudents :: [Record] -> [Student]
failingStudents records = [student | (student, grades) <- studentGrades, length (filter (< 3.0) grades) > 1]
  where
    studentRecords = groupBy (\(_, student, _, _) -> student) records
    studentSubjectRecords = [(student, groupBy (\(subject, _, _, _) -> subject) recs) | (student, recs) <- studentRecords]
    studentGrades = [(student, [averageNote recs | (_, recs) <- subjectRecs]) | (student, subjectRecs) <- studentSubjectRecords]


-- 4.
type Node = Int
type Edge = (Node, Node)
type AdjNode = (Node, [Node])
type Graph = [AdjNode]

-- Помощни функции:
nodes :: Graph -> [Node]
nodes = map fst

neighbors :: Graph -> Node -> [Node]
neighbors graph node = head [ns | (n, ns) <- graph, n == node]

dfs :: Graph -> Node -> [Node] -> [Node]
dfs graph node visited
  | node `elem` visited = visited
  | otherwise = foldr (dfs graph) (node : visited) (neighbors graph node)

-- Решение на задачата
buildGraph :: [Node] -> [Edge] -> Graph
buildGraph nodes edges = [(n, neighbors n) | n <- nodes]
  where
    neighbors node = [y | (x, y) <- edges, x == node] ++ [x | (x, y) <- edges, y == node]

reachableNodes :: Graph -> Node -> [Node]
reachableNodes graph node = dfs graph node []

connectedComponents :: Graph -> [[Node]]
connectedComponents graph = helper (nodes graph) [] []
  where
    helper [] _ ccs = ccs
    helper (n : ns) visited ccs
      | n `elem` visited = helper ns visited ccs
      | otherwise = helper ns (visited ++ cc) (cc : ccs)
      where
        cc = reachableNodes graph n

isFullyConnected :: Graph -> Bool
isFullyConnected graph = length (connectedComponents graph) == 1

diameter :: Graph -> Int
diameter graph = undefined -- TODO

nodesAtDistance :: Graph -> Int -> [(Node, Node)]
nodesAtDistance graph k = undefined -- TODO

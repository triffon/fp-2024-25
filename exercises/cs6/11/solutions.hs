import Prelude hiding (lookup)

g :: (Eq a, Num a) => [(a, [a])]
g =
  [ (1, [2, 3]),
    (2, [4, 1]),
    (3, [4]),
    (4, [5]),
    (5, [4])
  ]

data Nat -- от Natural number (естествено число)
  = Zero
  | Succ Nat
  deriving (Show)

-- Двоично дърво:
data BTree a
  = Leaf
  | Node a (BTree a) (BTree a)
  deriving (Show)

-- За дадено n връща n - 1
-- predNat от 0 е 0
predNat :: Nat -> Nat
predNat Zero = Zero
predNat (Succ n) = n

-- Превръщане на Integer в Nat
integerToNat :: Integer -> Nat
integerToNat 0 = Zero
integerToNat n = Succ (integerToNat (n - 1))

-- Превръщане на Nat в Integer
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

-- Събиране
plus :: Nat -> Nat -> Nat
plus Zero m = m
plus (Succ n) m = Succ (plus n m)

-- Умножение
mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ n) m = plus m (mult n m)

-- При делене на 0 операцията е неуспешна.
-- В противен случай искаме да върнем двойка от коефицент и остатък
--
-- Примери:
-- >>> safeDiv 5 0
-- Nothing
-- >>> safeDiv 13 5
-- Just (2, 3)
safeDiv :: Int -> Int -> Maybe (Int, Int)
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `quot` y, x `rem` y)

-- Проверява дали списък от списъци е квадратна матрица
isSquareMatrix :: [[a]] -> Bool
isSquareMatrix xss = all ((== length xss) . length) xss

-- Връща главния диагонал на матрица (списък от списъци)
mainDiag :: [[a]] -> [a]
mainDiag [] = []
mainDiag ([] : _) = []
mainDiag ((y : _) : xs) = y : mainDiag (map tail xs)

-- Връща вторичния диагонал на матрица (списък от списъци)
secondaryDiag :: [[a]] -> [a]
secondaryDiag = mainDiag . reverse

-- Търсим стойност по ключ в асоциативен списък (списък от двойки).
-- Може да не намерим такава.
--
-- Примери:
-- >>> lookup 5 [(10, 'a'), (5,'c')]
-- Just 'c'
-- >>> lookup 13 [(10, 'a'), (5,'c')]
-- Nothing
lookup :: Eq k => k -> [(k, v)] -> Maybe v
lookup _ [] = Nothing
lookup k (x : xs)
  | fst x == k = Just $ snd x
  | otherwise = lookup k xs

-- Връща броя наследници на даден връх
outDeg :: Eq a => [(a, [a])] -> a -> Int
outDeg nodes x = maybe 0 length $ lookup x nodes

-- Връща броя родители на даден връх
inDeg :: Eq a => a -> [(a, [a])] -> Int
inDeg x = foldl op 0
  where
    op acc (_, children)
      | x `elem` children = acc + 1
      | otherwise = acc

-- Проверява дали има ребро между два върха
edge :: Eq a => [(a, [a])] -> a -> a -> Bool
edge nodes a b = any isEdge nodes
  where
    isEdge (x, children) = x == a && b `elem` children

-- acyclic :: Eq a => [(a, [a])] -> Bool
-- acyclic nodes =

-- Проверява дали има път между два върха
-- TODO: възможно е да зациклим ако не пазим посетените върхове
path :: Eq a => [(a, [a])] -> a -> a -> Bool
path graph a b
  | a == b = True
  | otherwise =
      let succs = lookup a graph
       in case succs of
            Nothing -> False
            Just children -> any (\c -> path graph c b) children


data Tree a = Empty
            | Node a (Tree a) (Tree a)

-- Зад.1
-- Отново pattern matching to the rescue
rotateLeft, rotateRight :: Tree a -> Tree a
rotateLeft (Node p a (Node q b c)) = Node q (Node p a b) c
rotateRight (Node q (Node p a b) c) = Node p a (Node q b c)

-- Зад.2 - функцията, която се изисква по условие, е 90% от това
-- да направим нашето дърво функтор (останалото е синтаксис)
instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Empty = Empty
  fmap f (Node val left right) = Node (f val) (fmap f left) (fmap f right)

-- Зад.3
-- Проблем - когато имаме произволен брой поддървета, няма смисъл някои от тях да са празни
-- => създаваме си помощен тип за непразно дърво с произволен брой наследници,
-- всеки от които е също непразно дърво. Листата просто ще имат празен списък от наследници
data NonEmptyNTree a = NNode a [NonEmptyNTree a]
-- Основният тип - дървото може да е празно, но поддърветата не.
-- Всъщност еквивалентен на Maybe, но е важно да ползваме newtype за да покажем че е разли§ен тип.
newtype NTree a = NTree (Maybe (NonEmptyNTree a))

-- Зад.4
data BST a = BSTEmpty
           | BSTNode a (BST a) (BST a)

bstInsert :: Ord a => a -> BST a -> BST a
bstInsert x BSTEmpty = BSTNode x BSTEmpty BSTEmpty
bstInsert x (BSTNode val l r)
  | x < val   = BSTNode val (bstInsert x l) r
  | otherwise = BSTNode val l (bstInsert x r)
  
bstSearch :: Ord a => a -> BST a -> Bool
bstSearch _ BSTEmpty = False
bstSearch val (BSTNode x l r)
  | val == x   = True
  | val < x    = bstSearch val l
  | otherwise  = bstSearch val r

bstValues :: BST a -> [a] -- За тази задача няма нужда от ограничения :)
bstValues BSTEmpty = []
bstValues (BSTNode val l r) = bstValues l ++ [val] ++ bstValues r

bstSize :: BST a -> Int
bstSize BSTEmpty = 0
bstSize (BSTNode _ l r) = 1 + bstSize l + bstSize r

bstFromList :: Ord a => [a] -> BST a
bstFromList = foldr bstInsert BSTEmpty

bstSort :: Ord a => [a] -> [a]
bstSort = bstValues . bstFromList

-- Зад.5
-- Причини да НЕ ползваме синоним на BST (k,v) за Map:
-- - не всяко BST е валиден асоциативен списък (не трябва да се повтарят ключове)
-- - BST (k,v) изисква Ord v, което е ненужно
-- - бихме извиквали погрешка функции за BST над Map-ове, които да го направят невалидно
-- => създаваме си отделен тип
data Map k v = MEmpty
             | MNode k v (Map k v) (Map k v)

mapInsert :: Ord k => k -> v -> Map k v -> Map k v
mapInsert k newVal MEmpty = MNode k newVal MEmpty MEmpty
mapInsert k newVal (MNode key val l r)
  | k < key   = MNode key val (mapInsert key newVal l) r
  | k > key   = MNode key val l (mapInsert key newVal r)
  | otherwise = MNode key newVal l r

mapSearch :: Ord k => k -> Map k v -> Maybe v
mapSearch _ MEmpty = Nothing
mapSearch k (MNode key val l r)
  | k == key  = Just val
  | k < key   = mapSearch k l
  | otherwise = mapSearch k r

-- Зад.6
instance Functor (Map k) where
  fmap :: (a -> b) -> Map k a -> Map k b -- След fmap-ване ключовете остават същите
  fmap _ MEmpty = MEmpty
  fmap f (MNode key val l r) = MNode key (f val) (fmap f l) (fmap f r)

-- Зад.7
data Direction = L | R
{- Това поведение "разопаковай, приложи функция и опаковай"
-- е много често срещано - всъщност, това е Functor за типа Maybe.
consIfJust :: a -> Maybe [a] -> Maybe [a]
consIfJust _ Nothing   = Nothing
consIfJust x (Just xs) = Just (x:xs)

bstPath :: Ord a => a -> BST a -> Maybe [Direction]
bstPath _ BSTEmpty = Nothing
bstPath x (BSTNode val l r)
  | x == val  = Just []
  | x < val   = consIfJust L (bstPath x l)
  | otherwise = consIfJust R (bstPath x r)
-}

bstPath :: Ord a => a -> BST a -> Maybe [Direction]
bstPath _ BSTEmpty = Nothing
bstPath x (BSTNode val l r)
  | x == val  = Just []
  | x < val   = fmap (L:) (bstPath x l)
  | otherwise = fmap (R:) (bstPath x r)

-- Тази задача може да се реши и итеративно, без занимавки с Maybe-та
bstPath' :: Ord a => a -> BST a -> Maybe [Direction]
bstPath' x t = helper t []
  where helper BSTEmpty _ = Nothing
        helper (BSTNode val l r) path
          | x == val  = Just (reverse path)
          | x < val   = helper l (L:path)
          | otherwise = helper r (R:path)

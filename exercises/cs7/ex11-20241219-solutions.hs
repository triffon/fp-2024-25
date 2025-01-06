-- Примери за работа с алгебрични типове данни
data Student = Student String [Int] Int

-- Най-полезен е pattern matching, с който директно достъпваме "мембърите" на обекта
-- Понато и добре използвано (за списъци, наредени двойки)
getHighestGrade :: Student -> Int
getHighestGrade (Student fn grades _) = undefined

-- Тип с алтернативи - всяка една семантично различна, дори и с еднакво представяне
data Shape = Circle (Float,Float) Float
           | Rect (Float,Float) (Float,Float)
           | Square (Float,Float) Float
           | Unknown -- Някои алтернативи може да нямат "мембъри"

-- Хубавите функции могат да обработят всяка една алтернатива - ако не, може би сме сгрешили в дизайна на типа.
-- Заб.: Shape е типът, Circle/Square не са типове - те са начини, по които можем да получим обект от този тип.
area :: Shape -> Float
area (Circle _ r) = 3.14 * r * r
area (Rect (x1,y1) (x2,y2)) = (x2 - x1) * (y2 - y1)
area (Square _ a) = a * a
area Unknown = 0

-- Типовете могат да бъдат и шаблонни (най-често шаблонните параметри кръщаваме a,b,m както T,U,T1 в C++)
-- Отново - Tree само по себе си е шаблон, (Tree a) е инстанция на шаблона.
data Tree a = Empty
            | Node a (Tree a) (Tree a)

-- Един от най-простите и полезни алгебрични типове - за когато може да няма резултат от дадено изчисление.
-- Вградено в езика, дефинирано по следния начин
--data Maybe a = Nothing | Just a

-- Зад.1
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeUncons :: [a] -> Maybe (a,[a])
safeUncons [] = Nothing
safeUncons (x:xs) = Just (x,xs)

stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix [] y = Just y
stripPrefix x [] = Nothing
stripPrefix (x:xs) (y:ys)
  | x /= y    = Nothing
  | otherwise = stripPrefix xs ys

-- Ако напишем функцията така, че да върне "специална" стойност,
-- трябва после да я обработим където се извикваме рекурсивно.
-- Това обработване може да стане с локална променлива, помощна функция или case израз
findIndex1 :: Eq a => a -> [a] -> Int
findIndex1 _ [] = -1
findIndex1 y (x:xs)
  | x == y    = 0
  | otherwise = (case findIndex1 y xs of -1 -> -1
                                         n -> n + 1)

-- case изразът е най-полезен, защото може да използва pattern matching "по средата" на някоя друга функция
-- Всъщност е лесен преходът от хубава имплементация, обработваща специални стойности, към такава с Maybe.
findIndex' :: Eq a => a -> [a] -> (Maybe Int)
findIndex' _ [] = Nothing
findIndex' y (x:xs)
  | x == y    = Just 0
  | otherwise = (case findIndex' y xs of Nothing -> Nothing
                                         Just n -> Just $ n + 1)

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) = case f x of Nothing -> mapMaybe f xs
                                Just y -> y : mapMaybe f xs

-- Зад.2
maxSumPath :: (Num a, Ord a) => Tree a -> a
maxSumPath Empty = 0
maxSumPath (Node x l r) = x + max (maxSumPath l) (maxSumPath r)

-- Зад.3
-- Можем да напишем функция, която "разпознава" дали едно дърво е листо или не
-- или просто да използваме същия pattern там, където ни трябва тази проверка :)
isLeaf :: Tree a -> Bool
isLeaf (Node x Empty Empty) = True
isLeaf _ = False

prune :: Tree a -> Tree a
prune Empty = Empty
prune (Node x Empty Empty) = Empty
prune (Node x l r) = Node x (prune l) (prune r)

-- Зад.4
bloom :: Tree a -> Tree a
bloom Empty = Empty
bloom (Node x Empty Empty) = Node x l l
  where l = Node x Empty Empty -- същото като входа, хм - как да избегнем повторението? Бонус за който първи прочете това и ми отговори.
bloom (Node x l r) = Node x (bloom l) (bloom r)

-- Зад.5
-- Отново pattern matching to the rescue
rotateLeft, rotateRight :: Tree a -> Tree a
rotateLeft (Node p a (Node q b c)) = Node q (Node p a b) c
rotateRight (Node q (Node p a b) c) = Node p a (Node q b c)

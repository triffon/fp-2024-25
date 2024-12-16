import Prelude hiding (Either (..), Maybe (..), length)

-- Алгебрични Типове:
---------------------
-- С ключовата дума data можем да дефинираме алгебричен тип данни (ADT).
-- Алгебричен, защото се дефинира чрез "алгебрични" операции:

-- "Сума"
-- Това е нещо което би се постигнало с "enum" в други езици.
data Color
  = Red
  | Green
  | Blue
  deriving (Show) -- Show е типов клас, чийто елементи могат да се принтират.
  -- Този ред казва на Haskell да генерира нужните имплементации за Show.

-- "Произведение"
-- Малко като наредена n-торка
data Point = Point Int Int Int
  deriving (Show)

-- Нещо изглежда ли странно в тази дефиниция?

-- Колко възможни стойности има от тип Color и Point?

-- Конструктори:
----------------
-- Това което прави data е:
-- Въвежда типов конструктор и конструктори на данни
-- за съответния типов конструктор. Какво означава това?
--
-- 1) конструктор на типове - функция над типове
-- 2) конструктор на данни - (специална) функция над стойности

-- Дефиницията на списъците в Prelude, която ви бях дал:
-- 1) [] - празния списък
-- 2) x : xs - елемент x залепен за списък xs
--  * Важно е че ако x :: a, то xs :: [a]

-- Кои са конструктори на типове и кои са конструктори на данни?
-- TODO: type and kind
data List a
  = Nil
  | Cons a (List a)
  deriving (Show)

-- Празният списък е полиморфична константа

-- Конструкторите могат да се прилагат частично.

-- Като дефинирахме залепянето на елемент към списък префиксно,
-- започва да прилича на scheme:
someList :: List Int
someList = Cons 1 (Cons 2 (Cons 3 Nil))

-- А всъщност списъка в Prelude е дефиниран така:
-- data a [] = [] | a : [a]

-- Още типове:
--------------
-- Какво правят и как може да са ни полезни?

-- Maybe:
data Maybe a
  = Nothing
  | Just a
  deriving (Show)

-- Maybe е като конструктивно доказателство
-- т.е. ако Bool твърди дали нещо съществува или не,
-- то Maybe връща нещото ако то съществува
--
-- основно приложение на Maybe е да се използва за операции,
-- които могат да за провалят (а ние не харесваме exception-и)

-- Either:
data Either a b
  = Left a
  | Right b
  deriving (Show)

-- Двоично дърво:
data BTree a
  = Leaf
  | Node a (BTree a) (BTree a)
  deriving (Show)

-- Да пробваме да декларираме граф:
-- Всеки връх в графа има стойност и съседи.
data GNode a = GNode
  { value :: a,
    adjacents :: [a]
  }

-- Използваме record syntax, за да дадем имена на "полетата".
-- Освен това, сега Haskell ни генерира функции със същите имена,
-- с които можем да селектират съответните стойности:

-- >>> value (GNode 5 [])
-- 5

-- Съответно целият граф ще е списък от върховете:
data Graph1 a = Graph1 [GNode a]

-- Тук се вижда как само слагаме обвивка на вече съществуващ тип - [GNode a].
-- Има синтаксис (newtype) за този специален случай:
newtype Graph2 a = Graph2 [GNode a]

-- Или може просто да направим синоним на [GNode a]
type Graph a = [GNode a]

-- Въпрос: съседите трябва ли да са върхове?
--
-- Ако съседите са върхове, т.е. adjacents :: [GNode a]
-- То типа на графа е доста по-силен, защото още по време на компилация
-- се уверяваме че съседите наистина са върхове, а не някакви произволни
-- елементи от тип "a".
--
-- Как ще съсздадем стойност от тип Graph тогава?

-- Помните ли тази дефиниция на факториел и как зацикля за отрицателни числа?
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

-- Можем да избегнем това по 2 начина:
-- 1) Да правим проверка/преобразувание така че функцията да работи
--    за всички стойности от тип Int (т.е. правим по-силна имплементация)
-- 2) Да направим типа по-силен - вместо Int да ползваме естествени числа

data Nat -- от Natural number (естествено число)
  = Zero
  | Succ Nat
  deriving (Show)

-- NOTE: parse don't validate

-- Pattern matching ADTs:
-------------------------
-- Вече видяхме как можем да pattern match-ваме по (:)
-- >>> :t (:)
-- (:) :: a -> [a] -> [a]
-- То е някаква функция, какво й е специалното?
-- Защо не можем да правим pattern matching по (++) примерно?

-- Можем да pattern match-ваме само по конструкторите на ADT
mapList :: (a -> b) -> List a -> List b
mapList _ Nil = Nil
mapList f (Cons h t) = Cons (f h) (mapList f t)

-- Може да си мислим за имената на конструкторите
-- като табелки които стоят пред стойностите,
-- immutable низове, които съпоставяме като литерали (константи).

--------------------------------------------------------------------------------
-- ЗАДАЧИ --
------------

-- За дадено n връща (n - 1)
--
-- Примери:
-- >>> predNat (Succ (Succ Zero))
-- Succ Zero
-- >>> predNat Zero
-- Zero
predNat :: Nat -> Nat
predNat = undefined

-- Конвертиране на Integer в Nat
--
-- Примери:
-- >>> integerToNat 4
-- Succ (Succ (Succ (Succ Zero)))
integerToNat :: Integer -> Nat
integerToNat = undefined

-- Конвертиране на Nat в Integer
-- Трябва (integerToNat . natToInteger) да е идентитет за стойности от тип Nat
-- Обратното не е вярно!
natToInteger :: Nat -> Integer
natToInteger = undefined

-- Събиране за Nat
-- Не може да използвате natToInteger!
--
-- Примери:
-- >>> plus (integerToNat 1) (integerToNat 2)
-- Succ (Succ (Succ Zero))
plus :: Nat -> Nat -> Nat
plus = undefined

-- Умножение за Nat
-- Не може да използвате natToInteger!
--
-- Примери:
-- >>> natToInteger $ mult (integerToNat 3) (integerToNat 2)
-- 6
mult :: Nat -> Nat -> Nat
mult = undefined

-- При делене на 0 операцията е неуспешна.
-- В противен случай искаме да върнем двойка от коефицент и остатък
--
-- Примери:
-- >>> safeDiv 5 0
-- Nothing
-- >>> safeDiv 13 5
-- Just (2, 3)
safeDiv :: Int -> Int -> Maybe (Int, Int)
safeDiv = undefined

-- Проверява дали списък от списъци е квадратна матрица
isSquareMatrix :: [[a]] -> Bool
isSquareMatrix = undefined

-- Връща главния диагонал на матрица (списък от списъци)
mainDiag :: [[a]] -> [a]
mainDiag = undefined

-- Връща вторичния диагонал на матрица (списък от списъци)
secondaryDiag :: [[a]] -> [a]
secondaryDiag = undefined

-- Търсим стойност по ключ в асоциативен списък (списък от двойки).
-- Може да не намерим такава.
--
-- Примери:
-- >>> lookup 5 [(10, 'a'), (5,'c')]
-- Just 'c'
-- >>> lookup 13 [(10, 'a'), (5,'c')]
-- Nothing
lookup :: Eq k => k -> [(k, v)] -> Maybe v
lookup = undefined

-- Графи:
---------

-- NOTE: Ще използваме по-слабият тип за Graph,
--       тъй като той не ни носи никакви бонуси в сравнение с [(a,[a])],
--       направо ще работим със списъка от двойки (засега).

-- Връща броя наследници на даден връх
outDeg :: Eq a => a -> [(a, [a])] -> Int
outDeg = undefined

-- Връща броя родители на даден връх
inDeg :: Eq a => a -> [(a, [a])] -> Int
inDeg = undefined

-- Проверява дали има ребро между два върха
edge :: Eq a => a -> a -> [(a, [a])] -> Bool
edge = undefined

-- Проверява дали има път между два върха в ацикличен граф.
-- Бонус: да работи за графи с цикъли
path :: Eq a => [(a, [a])] -> a -> a -> Bool
path = undefined

-- Дървета:
-----------

-- Връща Just от първият елемент на списък, Nothing ако списъка е празен.
--
-- Примери:
-- >>> safeHead [1,2,3]
-- Just 1
-- >>> safeHead []
-- Nothing
safeHead :: [a] -> Maybe a
safeHead = undefined

-- Да се вмъкне елемент в дърво, спрямо наредбата на елементите му.
--
-- Примери:
-- >>> insertOrdered 5 Empty
-- Node 5 Empty Empty
-- >>> insertOrdered 5 (Node 10 Empty Empty)
-- Node 10 (Node 5 Empty Empty) Empty
-- >>> insertOrdered 5 (Node 3 Empty Empty)
-- Node 3 Empty (Node 5 Empty Empty)
insertOrdered :: Ord a => a -> BTree a -> BTree a
insertOrdered = undefined

-- Да се построи binary search tree от списък с елементи от тип "a"
-- (използвайте insertOrdered)
--
-- Примери:
-- >>> listToTree [1,10,2,9,3,8]
--
-- Node 8 (Node 3 (Node 2 (Node 1 Empty Empty) Empty) Empty)
--        (Node 9 Empty (Node 10 Empty Empty))
listToTree :: Ord a => [a] -> BTree a
listToTree = undefined

-- По дадено дърво от числа да се намери сбора на елементите му.
sumTree :: Num a => BTree a -> a
sumTree = undefined

-- Дали за всеки елемент в дърво е изпълнен даден предикат.
allTree :: (a -> Bool) -> BTree a -> Bool
allTree = undefined

-- Да се получи списък от обхождането на двоично дърво.
-- Нека обхождането да е ляво-корен-дясно
treeToList :: BTree a -> [a]
treeToList = undefined

-- Проверка дали елемент участва в дадено дърво.
--
-- Примери:
-- >>> elemTree 5 (listToTree [1..10])
-- True
-- >>> elemTree 42 (listToTree [1..10])
-- False
elemTree :: Eq a => a -> BTree a -> Bool
elemTree = undefined

-- Проверка за съществуване на елемент, изпълняващ даден предикат.
--
-- Примери:
-- >>> findPred even (listToTree [1..10])
-- Just 2
-- >>> findPred (>20) (listToTree [1..10])
-- Nothing
findPred :: (a -> Bool) -> BTree a -> Maybe a
findPred = undefined

-- За дадено двоично дърво от числа, намира максималната сума от числата
-- по някой път от корена до листо
maxSumPath :: (Num a, Ord a) => BTree a -> a
maxSumPath = undefined

-- Реализирайте функция, която за дадено балансирано двоично дърво
-- връща списък от низове, представляващи нивата на дървото
--
-- Пример:
bt :: BTree Int
bt =
  Node
    0
    ( Node
        1
        (Node 3 (Node 7 Leaf Leaf) Leaf)
        (Node 4 (Node 8 Leaf Leaf) Leaf)
    )
    ( Node
        2
        (Node 5 (Node 9 Leaf Leaf) (Node 10 Leaf Leaf))
        (Node 6 Leaf (Node 11 Leaf Leaf))
    )

-- >>> printBT bt
-- [“_____0________”,“__1_______2___”,“_3__4__5___6__”,“7__8__9_10__11”]

-- Или ако всеки елемент беше на нов ред:
-- >>> mapM_ putStrLn $ printBT bt
-- _____0________
-- __1_______2___
-- _3__4__5___6__
-- 7__8__9_10__11
--
-- Имайте предвид че броя на "_" зависи от броя на символите,
-- нужни за да се принтират елементите от по-високите нива на дървото
printBT :: (Show a) => BTree a -> [String]
printBT = undefined

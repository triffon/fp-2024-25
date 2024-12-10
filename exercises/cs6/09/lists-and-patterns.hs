import Prelude hiding (foldl, map, pi, reverse, splitAt, takeWhile, zip, zipWith)

-- pattern matching (съпоставяне на образци):
--------------------------------------------
-- Можем да правим нещо като cond в scheme - guards:
factGuards :: Int -> Int
factGuards n
  | n == 0 = 1
  | otherwise = n * factGuards (n - 1)

-- Сега същата функция, но дефинирана с case синтаксис:
factCase :: Int -> Int
factCase n = case n of
  0 -> 1
  k -> k * factCase (k - 1)

-- Какво ни прави впечатление?
-- При case не използваме булева проверка (== 0) за базовия случай,
-- a в общия случай преименувахме променливата от n на k.
-- Това е защото нещата от левия край на (->) в case синтаксиса са образци.

-- видове образци:
-- - литерал (константа) - съвпада с конкретна стойност
-- - променлива с име - съвпада с произволна стойност, свързва я с името
-- - анонимен образец "_" - съвпада с произволна стойност, не я свързва с име

-- Можем да дефинираме функции като поредица от равенства
-- (синтактична захар за case)

-- Все едно даваме различна дефиниция на функцията спрямо входните данни.
-- А вида на входните данни подбираме чрез образци.
--
-- Важно е да покрием всички случаи!

-- Примери:
fact :: Int -> Int
fact 0 = 1 -- получили сме 0 и директно връщаме 1
fact n = n * fact (n - 1)

-- получили сме някакво число и му даваме името "n".
-- със сигурност не е 0,
-- защото проверката на дефинициите се случва отгоре надолу.
-- Тоест, ако подадем отрицателно число - fact ще зацикли.

-- Дефиницията на списъците в Prelude:
-- 1) [] - празния списък
-- 2) x : xs - елемент x залепен за списък xs
--  * Важно е че ако x :: a, то xs :: [a]

-- Съответно можем да съпоставяме по [] и (:) за стандартните списъци
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

-- Не забравяйте за скобите в (x:xs)

-- map еквивалент на Scheme:
--
-- (define (map f xs)
--    (if (null? xs)
--      xs
--      (cons (f (car xs))
--            (map f (cdr xs)))))

-- Можем да правим иманувани образци с @
headPlusLength :: [Int] -> Int
headPlusLength [] = 0
headPlusLength r@(x : _) = x + length r

-- * Ползвайте guard-ове само когато наистина се налага (булева проверка)!

-- * Във всички други случаи ползвайте pattern matching!

-- Вложени дефиниции:
---------------------
-- let е израз, с който можем да правим вложени дефиниции:
-- let <def1>
--     <def2>
--      ...
--     <defn>
--  in <body>
--
-- Пример:
circlePerimeter :: Float -> Float
circlePerimeter r =
  let pi = 3.14
   in 2 * pi * r

-- let в Haskell е подобна конструкция на letrec в scheme

-- Друга конструкция е where - не е израз и работи само в рамките на дефиниция:
-- <function-definition>
--    where <def1>
--          <def2>
--           ...
--          <defn>
--
-- Пример:
circleArea :: Float -> Float
circleArea r = pi * r * r
  where
    pi = 3.14

-- * Внимавайте с идентацията!

-- Списъци и генератори:
------------------------
-- Списъците в Haskell са потоци,
-- тоест се оценяват мързеливо и могат да са безкрайни.

-- Съответно всички стандартни функции за списъци,
-- работят и върху безкрайни списъци.

-- List generators:

-- Можем да генерираме списъци от последователни елементи (числа, символи и др.)
-- Генераторите на списъци са синтактична захар за enumFrom и други функции.

-- Безкраен списък с елементи започващи от n:
-- enumFrom n ~~> [n,n+1,n+2,...]
-- [n..]      ~~> [n,n+1,n+2,...]

-- Безкраен списък с елементи започващи от n и всеки следващ със стъпка m-n=k
-- enumFromThen n m ~~> [n,m,m+k,m+2k,...]
-- [n,m..]          ~~> [n,m,m+k,m+2k,...]

-- Списък от елементите от n до m
-- enumFromTo n m ~~> [n,n+1,...,m-1,m]
-- [n..m]         ~~> [n,n+1,...,m-1,m]
-- [10..1]        ~~> []
-- не можем да правим списъци с низходящи елементи!

-- Списък от елементите от n до най-много k, и стъпка m-n
-- enumFromThenTo n m k = [n,m..k]
-- [1,3..10] == [1,3,5,7,9]             ~~> True
-- [10,9..1] == [10,9,8,7,6,5,4,3,2,1]  ~~> True
-- така вече можем да правим списъци с низходящи елементи.

-- List comprehension:
-- -------------------
-- [<expr> | {<generator>}+, {<condition>}+]

-- generator е от вида: <pattern> <- [a]
-- където <pattern> пасва на елемент от тип a

-- крайният резултат е списък от <expr> за всеки елемент получен от <generator>
-- за който са изпълнени всички условия

-- Пример:
-- [(x,y) | x<-[1..10], y<-[1..10], odd x, even y]
-- всички двойки (x,y), такива че
-- x е от [1..10], y е от [1..10], x е нечетно и y е четно

-- * всички двойки означава всяко със всяко когато имаме 2 генератора

-- TODO: Типови ограничения

--------------------------------------------------------------------------------
-- ЗАДАЧИ --
------------

-- TODO: примери

-- Типовете на елементите на списъка и акумулатора са различни
-- както ако в Scheme правите foldl върху списък от числа,
-- но с предикат и логическа връзка (and, or) натрупвате булева стойност
--
-- Примери:
-- >>> foldl (flip(:)) [] [1,2,3,4,5]
-- [5,4,3,2,1]
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl = undefined

-- За дадени два списъка [a1, ..., an] и [b1, ..., bn],
-- връща списъка от наредени двойки [(a1,b1), ..., (an,bn)]
-- Да се покрие и случаят, в който списъците имат различна дължина
--
-- Примери:
-- >>> zip [1,2] [3,4,5]
-- [(1,3), (2,4)]
--
-- >>> take 5 $ zip [0..] [0,-1..]
-- [(0,0),(1,-1),(2,-2),(3,-3),(4,-4)]
--
-- >>> take 5 $ zip [1..] ['a'..'z']
-- [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e')]
zip :: [a] -> [b] -> [(a, b)]
zip = undefined

-- Прилага поелементно функция върху двата списъка едновременно.
-- Връща списък от резултатите. Като map на 2 списъка в scheme
--
-- Примери:
-- >>> take 5 $ zipWith + [0..] [0,-1..]
-- [0,0,0,0,0]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = undefined

-- Връща най-дългият префикс на списък,
-- чийто елементи удоволетворяват дадения предикат
--
-- Примери:
-- >>> takeWhile (<5) [0..]
-- [0,1,2,3,4]
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile = undefined

-- Kato map, но очаква функция, която връща списък
-- и всички резултати са слети в един общ списък.
--
-- Примери:
-- >>> concatMap (\x -> [x,-x]) [1,2,3]
-- [1,-1,2,-2,3,-3]
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap = undefined

-- за списък от цели числа, премахва дубликатите,
-- т.е. запазва само първите срещания на даден елемент
--
-- Примери:
-- >>> nub [1,2,1,1,1,4,5,5,3,4,3]
-- [1,2,4,5,3]
nub :: (Eq a) => [a] -> [a]
nub = undefined

-- Проверява дали дадено число е просто
--
-- Примери:
-- >>> prime 0
-- False
--
-- >>> prime 1
-- False
--
-- >>> prime 2
-- True
--
-- >>> prime 3
-- True
--
-- >>> prime 4
-- False
prime :: Int -> Bool
prime = undefined

-- За дадено число n връща списък от първите n прости (положителни) числа
--
-- Примери:
-- >>> primes 6
-- [2,3,5,7,11,13]
primes :: Int -> [Int]
primes = undefined

-- (a,b,c) е Питагорова тройка ако: a*a + b*b = c*c
-- Да се напише функция която генерира безкраен списък
-- от всички Питагорови тройки.
--
-- Примери:
-- >>> take 7 pythagoreanTriples 
-- [(5,3,4),(5,4,3),(10,6,8),(10,8,6),(13,5,12),(13,12,5),(15,9,12)]
pythagoreanTriples :: [(Int, Int, Int)]
pythagoreanTriples = undefined

-- За дадено естествено число, връща списък от простите му делители
--
-- Примери:
-- >>> factorize 360
-- [2,2,2,3,3,5]
--
-- >>> sum $ map (length . factorize) $ primes 100
-- 100
factorize :: Int -> [Int]
factorize = undefined

-- Разделя списък на 2 части на даденият индекс
--
-- Примери:
-- >>> aplitAt 0 [1,2,3]
-- ([], [1,2,3])
--
-- >>> aplitAt 2 [1,2,3]
-- ([1,2], [3])
splitAt :: Int -> [a] -> ([a], [a])
splitAt = undefined

-- Измества елементите на списъка
--
-- Примери:
-- >>> shift 2 [1,2,3,4,5]
-- [3,4,5,1,2]
--
-- >>> shift (-2) [1,2,3,4,5]
-- [4,5,1,2,3]
shift :: Int -> [a] -> [a]
shift = undefined

-- За даден списък, генерира такъв списък, че на всяка поредица
-- от еднакви елементи в оригиналния списък в резултата съответства
-- наредена двойка (<елемент> . <брой-повторения>)
--
-- Примери:
-- >>> encode "aaaabbssbbbcc"
-- [('a',4),('b',2),('s',2),('b',3),('c',2)]
encode :: (Eq a) => [a] -> [(a, Int)]
encode = undefined

-- Имплементирайте едноименния алгоритъм за сортиране quicksort
--
-- Примери:
-- >>> quicksort [2,5,1,10,3,3,4]
-- [1,2,3,3,4,5,10]
quicksort :: (Ord a) => [a] -> [a]
quicksort = undefined

-- Имплементирайте едноименния алгоритъм за сортиране insertionSort
--
-- Примери:
-- >>> insertionSort [2,5,1,10,3,3,4]
-- [1,2,3,3,4,5,10]
insertionSort :: (Ord a) => [a] -> [a]
insertionSort = undefined

-- За даден списък, връща списък от всички негови подредици
--
-- Примери:
-- >>> subsequences [1,2,3]
-- [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]
subsequences :: [a] -> [[a]]
subsequences = undefined

-- За даден списък, връща списък от всичките му пермутации
--
-- Примери:
-- >>> permutations [1,2,3]
-- [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
permutations :: [a] -> [[a]]
permutations = undefined

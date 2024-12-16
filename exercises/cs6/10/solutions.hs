import Prelude hiding (cycle, iterate, repeat, scanl)

-- Безкраен списък от всички естествени числа.
-- Имплементирайте чрез рекурсия.
--
-- Бонус: Без помощна функция(променлива)
--
-- Пример:
-- >>> take 10 nats
-- [0,1,2,3,4,5,6,7,8,9]
nats :: [Integer]
nats = 0 : map succ nats

-- Безкрайно повторение на стойност
--
-- Пример:
-- >>> take 5 $ repeat 'a'
-- "aaaaa"
repeat :: a -> [a]
repeat x = x : repeat x

-- За функция f и аргумент x,
-- генерира безкрайният списък [x, f(x), f(f(x)), ...]
--
-- Пример:
-- >>> take 5 $ iterate (^2) 2
-- [2,4,16,256,65536]
iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

-- За даден списък xs,
-- генерира безкрайният списък от повторение на елементите на xs
--
-- Пример:
-- >>> take 8 $ cycle [1,2,3]
-- [1,2,3,1,2,3,1,2]
cycle :: [a] -> [a]
cycle xs = xs ++ cycle xs

-- Вариант на foldl, в който пазим междинните резултати от всяка стъпка
-- и връщаме списък от тях, вместо само финалният резултат.
-- (понякога доста полезно за дебъгване на foldl)
--
-- Пример:
-- >>> scanl (+) 0 [1..10]
-- [0,1,3,6,10,15,21,28,36,45,55]
scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl _ acc [] = [acc]
scanl op acc (x : xs) = acc : scanl op (op acc x) xs

-- Безкраен поток от числата на Фибоначи (hint: zipWith)
--
-- Пример:
-- >>> take 10 fibs
-- [0,1,1,2,3,5,8,13,21,34]
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

-- Безкраен списък, в който n-тия елемент е n! (факториел)
-- Бонус: Опитайте да я имплементирате чрез scanl.
--
-- Пример:
-- >>> take 7 factsScanl
-- [1,1,2,6,24,120,720]
facts :: [Integer]
facts = map (\n -> product [1 .. n]) [0 ..]

factsScanl :: [Integer]
factsScanl = scanl (*) 1 (drop 1 nats)

-- Безкраен списък от всички прости числа.
-- Имплементирайте чрез алгоритъма "Решето на Ератостен"
--
-- Пример:
-- >>> take 10 primes
-- [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
primes :: [Integer]
primes = next [2 ..]
  where
    next (x : xs) = x : next (filter ((/= 0) . (`rem` x)) xs)

-- (a,b,c) е Питагорова тройка ако: a*a + b*b = c*c
-- Да се напише функция която генерира безкраен списък
-- от всички Питагорови тройки.
pythagoreanTriples :: [(Int, Int, Int)]
pythagoreanTriples =
  [(a, b, c) | a <- [1 ..], b <- [1 .. a], c <- [1 .. a], p a b c]
  where
    p x y z = x ^ 2 == y ^ 2 + z ^ 2

-- Безкраен списък от всички двойки естествени числа.
-- Имплементирайте като обединение на диагоналите.
--
-- Пример:
-- >>> take 10 natTuples
-- [(0,0),(0,1),(1,0),(0,2),(1,1),(2,0),(0,3),(1,2),(2,1),(3,0)]
natTuples :: [(Int, Int)]
natTuples = concatMap diag [0 ..]
  where
    diag n = [(x, y) | x <- [0 .. n], y <- [0 .. n], x + y == n]

-- Генерирайте безкрайния поток от низове, представляващ всички композиции на
-- функции f и g:
-- ["id", "id . f", "id . g", "id . f . f", "id . f . g", "id . g . f", ...]
compositions :: String -> String -> [String]
compositions f g = next ["id"]
  where
    next xs = xs ++ next (concatMap (\x -> [x ++ " . " ++ f, x ++ " . " ++ g]) xs)

-- По дадени 2 безкрайни списъка и двуместен предикат, да се намери
-- безкрайният списък от двойки елементи от двата списъка изпълняващи предиката
filterTuples :: (a -> b -> Bool) -> [a] -> [b] -> [(a, b)]
filterTuples p xs ys = filter (uncurry p) cartesian
  where
    cartesian = map (\(x, y) -> (xs !! x, ys !! y)) natTuples

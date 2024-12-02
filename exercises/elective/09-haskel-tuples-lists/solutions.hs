-- 1.
lenght' :: [a] -> Int
lenght' [] = 0
lenght' (_:xs) = 1 + lenght' xs

-- 2.
exists :: (a -> Bool) -> [a] -> Bool
exists _ [] = False
exists p (x:xs) = p x || exists p xs

exists' :: (a -> Bool) -> [a] -> Bool
exists' p xs = foldr (||) False (map p xs)

-- 3.
forall' :: (t -> Bool) -> [t] -> Bool
forall' _ [] = True
forall' p (x:xs) = p x && forall' p xs

forall'' :: (a -> Bool) -> [a] -> Bool
forall'' p xs = foldr (&&) True (map p xs)

-- 4.
member :: (Eq a) => a -> [a] -> Bool
member x = exists (== x)

-- 5.
push :: t -> [t] -> [t]
push x [] = [x]
push x (y:ys) = y:(push x ys)

-- `foldr` magic/incantation
push' :: a -> [a] -> [a]
push' x = foldr (:) [x]

-- 6.
-- Решение с рекурсия и помощна функция
reverse' :: [a] -> [a]
reverse' xs = helper xs []
    where
        helper [] acc = acc
        helper (x:xs) acc = helper xs (x:acc)

-- `foldr` magic/incantation
reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

-- 7.
init' :: [a] -> [a]
init' [] = error "init not defined for empty list"
init' [_] = []
init' (x:xs) = x:init' xs

-- 8.
insert :: a -> Int -> [a] -> [a]
insert x _ [] = [x]
insert x 0 ys = x:ys
insert x n (y:ys) = y : (insert x (n - 1) ys)

-- 9.
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ acc []     = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)

-- 10.
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc []     = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs

-- 11.
product' :: (Num a) => [a] -> a
product' = foldr (*) 1

-- 12.
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

-- Решение със `zipWith`
zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith (,)

-- 13.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- Решение със `zip`
zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = [f x y | (x, y) <- zip xs ys]

-- 14.
inteleave :: [a] -> [a] -> [a]
inteleave [] [] = []
inteleave (x:xs) (y:ys) = [x, y] ++ inteleave xs ys
inteleave _ _ = error "Lists must be of equal size"

-- 15.
nats :: (Integral a) => [a]
nats = 1:map (+1) nats

-- 16.
pythagoreanTriples :: (Integral a) => [(a, a, a)]
pythagoreanTriples = [(x, y, z) | z <- [1 ..], y <- [1 .. z], x <- [1 .. y], x^2 + y^2 == z^2]

-- 17.
fibs :: (Integral a) => [a]
fibs = 0:1:zipWith (+) fibs (tail fibs)

-- 18.
primes :: (Integral a) => [a]
primes = sieve [2 ..] where
    sieve (x:xs) = x: sieve [z | z <- xs, z `mod` x /= 0]

-- По-оптимално решение, което не използва остатък (`mod`) и филтрира само необходимите числа
filterOut :: (Ord a) => [a] -> [a] -> [a]
filterOut [] _ = []
filterOut xs [] = xs
filterOut a@(x:xs) b@(y:ys)
    | x < y     = x : filterOut xs b
    | x == y    = filterOut xs ys
    | otherwise = filterOut a ys

primes' :: (Integral a) => [a]
primes' = sieve [2 ..] where
    sieve (x:xs) = x: sieve (filterOut xs [x^2, x^2 + x ..])

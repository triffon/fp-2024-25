module Lists where

import Prelude hiding (length, head, enumFromTo, (++),
                       reverse, (!!), elem, init, last, take, drop,
                       map, filter, foldr, foldl, foldr1, foldl1, scanr, scanl,
                       zip, unzip, zipWith, takeWhile, dropWhile, any, all)
import Text.XHtml (h1, background, label)
import Basics (x)

-- >>> !!! :t (1:2):3
-- (1:2):3 :: (Num a, Num [a], Num [[a]]) => [[a]]

-- >>> :t []
-- [] :: [a]

-- >>> :t ""
-- "" :: String

-- >>> "" == []
-- True

{-
length []    = 0
length (_:t) = 1 + length t
-}

length = foldr (const (1+)) 0

-- >>> length [1,2,3]
-- 3

head (h:_) = h

-- >>> head [1,2,3]
-- 1

-- >>> [1..10]
-- [1,2,3,4,5,6,7,8,9,10]

-- >>> [1..1]
-- [1]

-- >>> [1..0]
-- []

enumFromTo :: Int -> Int -> [Int]
enumFromTo a b
  | a > b     = []
  | otherwise = a : enumFromTo (a + 1) b

-- >>> enumFromTo 1 10
-- [1,2,3,4,5,6,7,8,9,10]

-- >>> [1,4..15]
-- [1,4,7,10,13]

-- >>> [1,0 .. -5]
-- [1,0,-1,-2,-3,-4,-5]

-- !!! [1,1.. 5]
-- !!! [1,1.. 1]

(++) :: [a] -> [a] -> [a]
{-
[]     ++ l = l
(x:xs) ++ l = x : xs ++ l
-}

{-
flip f x y = f y x
-}

-- (++) l1 l2 = foldr (:) l2 l1 
(++) = flip (foldr (:)) 
-- >>> [1..3] ++ [5..7]
-- [1,2,3,5,6,7]

reverse :: [a] -> [a]
{-
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]
-}

-- reverse = foldr (\x r -> r ++ [x]) []
--reverse = foldr (\x -> (++[x])) []

-- reverse = foldl (\r x -> x : r) []
reverse = foldl (flip (:)) []

-- >>> reverse [1..5]
-- [8,7,6,5,4,3,2,1]


(x:_)  !! 0 = x
[]     !! n = error "Невалиден индекс"
(_:xs) !! n = xs !! (n - 1)

-- >>> [1..5] !! 2
-- 3

-- >>> [1..5] !! 10
-- Невалиден индекс

elem :: Eq t => t -> [t] -> Bool
{-
elem _ [] = False
elem y (x:xs)
 | x == y    = True
 | otherwise = elem y xs  
-}

{-
elem _ []     = False
elem y (x:xs) = x == y || elem y xs
-}

--elem y = foldr (\x r -> x == y || r) False
elem y = foldr (\x -> (x == y ||)) False


-- >>> 3 `elem` [1..5]
-- True

-- >>> elem 7 [1..5]
-- False

-- >>> :t max
-- max :: Ord a => a -> a -> a

-- >>> show 1.2
-- "1.2"

-- >>> show [1,2,3]
-- "[1,2,3]"

-- >>> show (+)
-- No instance for `Show (Integer -> Integer -> Integer)'
--   arising from a use of `show'
--   (maybe you haven't applied a function to enough arguments?)
-- In the expression: show (+)
-- In an equation for `it_a11sH': it_a11sH = show (+)

l :: [Int]
l = read "[1]"

-- >>> l
-- [1]


-- >>> (read "[1]")::[Int]
-- [1]

-- >>> max [1,2] [2,3]
-- [2,3]

pythagoreanTriples a b = [ (x, y, z) | x <- [a..b], y <- [x..b], z <- [y..b],
                            x^2 + y^2 == z^2, gcd x y == 1 ]
-- >>>  pythagoreanTriples 1 100
-- [(3,4,5),(5,12,13),(7,24,25),(8,15,17),(9,40,41),(11,60,61),(12,35,37),(13,84,85),(16,63,65),(20,21,29),(28,45,53),(33,56,65),(36,77,85),(39,80,89),(48,55,73),(65,72,97)]

pythagoreanTriples' :: Int -> Int -> [(Int,Int,Int)]  
pythagoreanTriples' a b =
    concatMap (filter (\(x,y,_) -> gcd x y == 1) . 
                filter (\(x,y,z) -> x^2+y^2==z^2) . 
                    (\x ->
                        concatMap (\y ->
                            map (x,y,) [y..b])
                        [x..b])) 
                    [a..b]

-- >>> pythagoreanTriples' 1 100
-- [(3,4,5),(5,12,13),(7,24,25),(8,15,17),(9,40,41),(11,60,61),(12,35,37),(13,84,85),(16,63,65),(20,21,29),(28,45,53),(33,56,65),(36,77,85),(39,80,89),(48,55,73),(65,72,97)]

init :: [a] -> [a]
init [_] = []
init (x:xs) = x:init xs

-- >>> init [1..5]
-- [1,2,3,4]

-- >>> init []
-- /home/trifon/fmisync/Courses/2024_25/FP_2024_25/fp-2024-25/lectures/haskell/Lists.hs:(134,1)-(135,23): Non-exhaustive patterns in function init

last :: [a] -> a
{-
last [x] = x
last (_:xs) = last xs
-}
-- last = foldr1 (\_ r -> r)
-- last = foldr1 (const id)
last = foldl1 (const id)

-- >>> foldl1 (\_ x -> x) [1..10]
-- >>> foldl1 (const id) [1..10]
-- 10

-- >>> last [1..5]
-- 5

take :: Int -> [a] -> [a]
{-
take 0 _  = []
take _ [] = []
take n (x:xs) = x:take (n-1) xs 
-}

-- take n = zipWith (\i x -> x) [1..n]
take n = zipWith (const id) [1..n]

-- >>> take 2 [1..10]
-- [1,2]

drop :: Int -> [a] -> [a]
drop 0 l = l
drop _ [] = []
drop n (_:xs) = drop (n-1) xs


-- >>> drop 2 [1..10]
-- [3,4,5,6,7,8,9,10]

-- >>> concat [[1,2,3],[4,5,6],[7,8,9]]
-- [1,2,3,4,5,6,7,8,9]

plus1 = map (+1)

-- >>> plus1 [1..5]
-- [2,3,4,5,6]

{-
map :: (t -> a) -> [t] -> [a]
map _ [] = []
map f (x:xs) = let r = map f xs in f x : r
-}

map :: (t -> a) -> [t] -> [a]
map f = foldr (\x -> (f x:)) []

filter :: (a -> Bool) -> [a] -> [a]
{-
filter _ []     = []
filter p (x:xs)
 | p x = x:r
 | otherwise = r
 where r = filter p xs
-}

-- filter p = foldr (\x r -> if p x then x : r else r) []
filter p = foldr (\x -> if p x then (x:) else id) []

oddOnly :: [Integer] -> [Integer]
oddOnly = filter odd

-- >>> oddOnly [1..10]
-- [1,3,5,7,9]


-- >>> [(x, y) | x <- [1..3], y <- [x..6]]
-- [(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(2,2),(2,3),(2,4),(2,5),(2,6),(3,3),(3,4),(3,5),(3,6)]

-- >>> concatMap (\x -> map (\y -> (x,y)) [x..6]) [1..3]
-- [(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(2,2),(2,3),(2,4),(2,5),(2,6),(3,3),(3,4),(3,5),(3,6)]

foldr :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
foldr _ nv [] = nv
foldr op nv (x:xs) = x `op` foldr op nv xs

-- >>> foldr (+) 0 [1..10]
-- 55

foldl :: (t1 -> t2 -> t1) -> t1 -> [t2] -> t1
foldl _ nv [] = nv
foldl op nv (x:xs) = foldl op (nv `op` x) xs

-- >>> foldl (+) 0 [1..10]
-- 55

foldr1 :: (t -> t -> t) -> [t] -> t
foldr1 _ [] = error "Списъкът е празен"
foldr1 _ [x] = x
foldr1 op (x:xs) = x `op` foldr1 op xs

-- >>> foldr1 max [1..5]
-- 5

foldl1 _ [] = error "Списъкът е празен"
foldl1 op (x:xs) = foldl op x xs

-- >>> scanr (+) 0 [1..10]
-- [55,54,52,49,45,40,34,27,19,10,0]

scanr :: (t -> a -> a) -> a -> [t] -> [a]
{-
scanr _ nv [] = [nv]
scanr op nv (x:xs) = x `op` r : rest
  where rest@(r:_) = scanr op nv xs
-}

scanr op nv = foldr (\x rest@(r:_) -> x `op` r : rest) [nv]

-- >>> scanl (+) 0 [1..10]
-- [0,1,3,6,10,15,21,28,36,45,55]

scanl :: (t1 -> t2 -> t1) -> t1 -> [t2] -> [t1]
scanl _ nv [] = [nv]
scanl op nv (x:xs) = nv : scanl op (nv `op` x) xs

-- >>> zip [1..5] [3..10]
-- [(1,3),(2,4),(3,5),(4,6),(5,7)]

zip :: [a] -> [b] -> [(a, b)]
{-
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y):zip xs ys
-}
zip = zipWith (,)

-- >>> unzip [(1,3),(2,4),(3,5),(4,6),(5,7)]
-- ([1,2,3,4,5],[3,4,5,6,7])

unzip :: [(a1, a2)] -> ([a1], [a2])
{-
unzip [] = ([], [])
unzip ((x,y):xys) = ( x : xs, y : ys )
  where (xs,ys) = unzip xys
-}

-- unzip xys = (map fst xys, map snd xys)

unzip = foldr (\(x,y) (xs,ys) -> (x:xs, y:ys )) ([], [])

-- >>> zipWith (+) [1..5] [2..6]
-- [3,5,7,9,11]

zipWith :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
zipWith _  [] _ = []
zipWith _  _  [] = []
zipWith op (x:xs) (y:ys) = x `op` y:zipWith op xs ys

-- >>> takeWhile (<3) [1..10]
-- [1,2]

-- takeWhile p = foldr (\x r -> if p x then x : r else []) []
takeWhile p = foldr (\x -> if p x then (x:) else const []) []

-- filter p = foldr (\x r -> if p x then x : r else r) []
-- filter p = foldr (\x -> if p x then (x:) else id) []

-- >>> dropWhile (<3) [1..10]
-- [3,4,5,6,7,8,9,10]

dropWhile :: (a -> Bool) -> [a] -> [a]
-- !!! dropWhile p = foldr (\x r -> if p x then r else x:r) []  
dropWhile _ [] = []
dropWhile p (x:xs)
 | p x = dropWhile p xs
 | otherwise = xs

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p l = foldr (\l@(x:xs) ys -> if p x then ys else l) [] (init (scanr (:) [] l))

-- >>> dropWhile' (<3) [1..10]
-- [3,4,5,6,7,8,9,10]

-- >>> any odd [1..10]
-- True

-- >>> all odd [1..10]
-- False

-- >>> all (>0) [1..10]
-- True

any :: (t -> Bool) -> [t] -> Bool
any p l = or (map p l) 

all :: (t -> Bool) -> [t] -> Bool
all p l = and (map p l)

-- >>> zip [1..5] [2..5]
-- [(1,2),(2,3),(3,4),(4,5)]

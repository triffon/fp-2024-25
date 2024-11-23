module Lists where

import Prelude hiding (length, head, enumFromTo, (++),
                       reverse, (!!), elem, init, last, take, drop)
import Basics (x)

-- >>> !!! :t (1:2):3
-- (1:2):3 :: (Num a, Num [a], Num [[a]]) => [[a]]

-- >>> :t []
-- [] :: [a]

-- >>> :t ""
-- "" :: String

-- >>> "" == []
-- True

length []    = 0
length (_:t) = 1 + length t

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
[]     ++ l = l
(x:xs) ++ l = x : xs ++ l

-- >>> [1..3] ++ [5..7]
-- [1,2,3,5,6,7]

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

-- >>> reverse [1..5]
-- [5,4,3,2,1]

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

elem _ []     = False
elem y (x:xs) = x == y || elem y xs


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

init :: [a] -> [a]
init [_] = []
init (x:xs) = x:init xs

-- >>> init [1..5]
-- [1,2,3,4]

-- >>> init []
-- /home/trifon/fmisync/Courses/2024_25/FP_2024_25/fp-2024-25/lectures/haskell/Lists.hs:(134,1)-(135,23): Non-exhaustive patterns in function init

last :: [a] -> a
last [x] = x
last (_:xs) = last xs

-- >>> last [1..5]
-- 5

take :: Int -> [a] -> [a]
take 0 _  = []
take _ [] = []
take n (x:xs) = x:take (n-1) xs 

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

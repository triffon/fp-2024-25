{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}
-- no incomplete patterns in lambdas!

import Prelude hiding (abs, filter, map, reverse)

abs :: Int -> Int
abs n = if n >= 0 then n else -n

-- (->) е дясно асоциативна, тоест типа на apply можем да го запишем така:
-- apply :: (a -> b) -> (a -> b)
-- id :: a -> a
-- но а е произволен тип, в частност (a -> b)
apply' :: (a -> b) -> a -> b
apply' = id

-- това връща функция защото се прилага частично (x не се подава)
-- т.е. се извиква така и резултата е функция която чака
-- още един аргумент да бъде подаден
-- Пример: compose (^2) succ
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

-- Помните функциите take и drop
-- В Haskell са вградени
prefix :: [Int] -> [Int] -> Bool
prefix xs ys = take (length xs) ys == xs

suffix :: [Int] -> [Int] -> Bool
suffix xs ys = xs == drop (length ys - length xs) ys

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x acc -> if p x then x : acc else acc) []

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x acc -> f x : acc) []

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

weakListComprehension :: (a -> b) -> (a -> Bool) -> [a] -> [b]
weakListComprehension f p = map f . filter p

closed :: (Int -> Int) -> [Int] -> [Int]
closed f xs = filter (\x -> f x `elem` xs) xs

-- Може и без pattern matching по акомулираната стойност (с fst и snd)
uninterleave :: [a] -> ([a], [a])
uninterleave = foldr (\x (xs, ys) -> (x : ys, xs)) ([], [])

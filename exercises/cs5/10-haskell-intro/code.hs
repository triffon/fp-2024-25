{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors

foo :: Float
foo = 2.5

bar :: Bool
bar = True

qux :: Char
qux = 'q'

add1 :: Int -> Int
add1 x = x + 1

add :: Int -> Int -> Int
-- add = \x -> \y -> x + y
add x y = x + y

duplicate :: (a -> a) -> a -> a
duplicate f x = f (f x)

right :: baba -> dyado -> dyado
right _ y = y

tripleEq :: (Eq a) => a -> a -> a -> Bool
tripleEq x y z = (x == y) && (y == z)

sumNums :: [Int] -> Int
sumNums [] = 0
sumNums (h:t) = h + (sumNums t)

sumNumsNonNeg :: [Int] -> Int
sumNumsNonNeg [] = 0
sumNumsNonNeg (h:t)
    | h < 0     = sumNumsNonNeg t
    | otherwise = h + (sumNumsNonNeg t)

vecLen :: (Float, Float) -> Float
vecLen (a, b) = sqrt (a ^ 2 + b ^ 2)

normalise :: (Float, Float) -> (Float, Float)
normalise v@(a, b) = (a / l, b / l)
    where
        l = vecLen v

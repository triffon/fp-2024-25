{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors

foo :: [[Int]]
foo = [[42, 26], [2, 3]]

add1 :: (Num n) => n -> n
add1 x = x + 1

add2 :: Int -> (Int -> Int)
add2 = \x -> (\y -> (x + y))

ret42 :: foo -> Int
ret42 _x = 42

len :: [a] -> Int
len [] = 0
len (_h:t) = 1 + len t

vecLen :: (Float, Float) -> Float
vecLen (a, b) = sqrt (a ** 2 + b ** 2)

normalize :: (Float, Float) -> (Float, Float)
normalize v@(a, b) = (a / l, b / l)
    where
        l = vecLen v

negate :: Bool -> Bool
negate True = False
negate False = True

ageOf :: String -> Int
ageOf name
    | name == "pesho" || name == "penka" = 42
    | name == "gosho" = 26
    | otherwise = 0

add1toall :: [Int] -> [Int]
add1toall [] = []
add1toall (h:t) = (1 + h) : (add1toall t)

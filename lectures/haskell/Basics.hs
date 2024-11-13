module Basics where

x :: Int
x = 2

-- >>> x
-- 7

-- >>> -x
-- -2

y :: Double
y = (fromIntegral x)^2 + 7.5

-- >>> y^2
-- 132.25

z :: String
z = "Hello, Haskell!"

z2 :: String
z2 = show x ++ show y

-- >>> z2
-- "211.5"

u = 2

v = u^2 + 3.5
-- >>> :t u
-- u :: Double

-- >>> :t v
-- v :: Double
-- >>> v
-- 7.5

-- >>> :t (^)
-- (^) :: (Num a, Integral b) => a -> b -> a

-- >>> :t (^^)
-- (^^) :: (Fractional a, Integral b) => a -> b -> a

-- >>> :t (**)
-- (**) :: Floating a => a -> a -> a

-- twice :: (Int -> Int) -> Int
twice :: (t -> t) -> t -> t
twice f x = f (f x)

-- >>> twice (twice (1+)) 2
-- 6

-- diag :: (t -> t -> t) -> t -> t
diag :: (t1 -> t1 -> t2) -> t1 -> t2
diag f x = f x x

square :: Int -> Int
square x = x * x
-- >>> square (-x)
-- 4

-- >>> subtract 5 3
-- -2

-- >>> diag (+) 2 
-- 4

-- >>> (8-) 5
-- 3

-- >>> if x > 5 then x + 2 else "Error"
-- Couldn't match type `[Char]' with `Int'
-- Expected: Int
--   Actual: String
-- In the expression: "Error"
-- In the expression: if x > 5 then x + 2 else "Error"
-- In an equation for `it_aOaO':
--     it_aOaO = if x > 5 then x + 2 else "Error"

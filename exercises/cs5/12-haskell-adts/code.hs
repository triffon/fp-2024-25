{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors
{-# LANGUAGE GHC2021 #-}

import Data.List.Extra (snoc)

data MyBool
    = MyTrue
    | MyFalse
    deriving (Show, Eq)

mynot :: MyBool -> MyBool
mynot MyTrue = MyFalse
mynot _ = MyTrue

data Shape n
    = Circle n
    | Rectangle n n
    | Ngon Int n
    | Point
    | UnionShape (Shape n) (Shape n)
    deriving (Show)

area :: (Floating n) => (Shape n) -> n
area Point = 0
area (Rectangle a b) = a * b
area (Circle r) = r * r * pi
area (UnionShape s1 s2) = area s1 + area s2
area (Ngon _ _) = error "I am lazy"

data List a
    = Cons a (List a)
    | Empty
    deriving (Show)

lsum :: (Num n) => List n -> n
lsum Empty = 0
lsum (Cons x xs) = x + lsum xs

data Colour
    = Cyan
    | Magenta
    | Yellow
    deriving (Show)

class MyEq a where
    equals :: a -> a -> MyBool

instance MyEq Colour where
    equals Cyan Cyan = MyTrue
    equals Magenta Magenta = MyTrue
    equals Yellow Yellow = MyTrue
    equals _ _ = MyFalse

instance Eq Colour where
    c1 == c2 = equals c1 c2 == MyTrue

instance Ord Colour where
    Cyan <= _ = True
    Magenta <= Magenta = True
    Magenta <= Yellow = True
    Yellow <= Yellow = True
    _ <= _ = False



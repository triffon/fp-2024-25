{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors

data MyBool
    = MyTrue
    | MyFalse
    deriving (Show, Eq)

data Colour
    = Red
    | Green
    | Cyan
    | Magenta
    | Yellow
    | Blue
    deriving (Show)

data Shape
    = Point
    | Circle Float
    | Rectangle Float Float
    | Ngon Int Float

area :: Shape -> Float
area Point = 0
area (Circle r) = r * r * pi
area (Rectangle a b) = a * b
area (Ngon _n _h) = error "I am too lazy"

instance Show Shape where
    show Point = "point"
    show (Circle r) = "circle with radius " ++ (show r)
    show (Rectangle a b) = "rectangle "
        ++ (show a) ++ "x" ++ (show b)
    show (Ngon n h) = "ngon with " ++ (show n)
        ++ " sides and h=" ++ (show h)

data GShape n
    = GPoint
    | GCircle n
    | GRectangle n n
    | GNgon Int n
    deriving (Show)

garea :: Floating n => GShape n -> n
garea GPoint = 0
garea (GCircle r) = r * r * pi
garea (GRectangle a b) = a * b
garea (GNgon _n _h) = error "I am too lazy"

data List a
    = Cons a (List a)
    | Empty
    deriving (Show)

lsum :: (Num a) => List a -> a
lsum Empty = 0
lsum (Cons x xs) = x + lsum xs

class Countable a where
    count :: a -> Int

instance Countable (List a) where
    count Empty = 0
    count (Cons _ xs) = 1 + count xs

instance Countable [a] where
    count [] = 0
    count (_:xs) = 1 + count xs

-- class Eq a where
--     (==) :: a -> a -> Bool

instance Eq a => Eq (List a) where
    Empty == Empty = True
    (Cons x xs) == (Cons y ys) = (x == y) && (xs == ys)
    _ == _ = False

instance Ord a => Ord (List a) where
    Empty <= _ = True
    (Cons x xs) <= (Cons y ys)
        | x == y = xs <= ys
        | x < y = True
        | otherwise = False
    (Cons _ _) <= Empty = False

module Data where

-- >>> :t [False, True]
-- [False, True] :: [Bool]

-- >>> :k Bool
-- Bool :: *

-- >>> :k [Bool]
-- [Bool] :: *
type UnaryFunction a = a -> a

-- >>> :k UnaryFunction Int
-- UnaryFunction Int :: *

-- >>> :k UnaryFunction
-- UnaryFunction :: * -> *

type Matrix a = [[a]]

-- >>> :k Matrix Int
-- Matrix Int :: *

-- >>> :k Matrix (UnaryFunction Int)
-- Matrix (UnaryFunction Int) :: *

-- >>> :k Matrix
-- Matrix :: * -> *

type Dictionary k v = [(k, v)]

-- >>> :k Dictionary Int String
-- Dictionary Int String :: *

-- >>> :k Dictionary
-- Dictionary :: * -> * -> *

-- >>> :k Dictionary Int
-- Dictionary Int :: * -> *

-- >>> :k ([a] -> Integer)
-- ([a] -> Integer) :: *

-- >>> :k forall a . ([a] -> Integer)
-- forall a . ([a] -> Integer) :: *

--- >>> :t 5
-- 5 :: Num a => a

-- >>> :t 5.0
-- 5.0 :: Fractional a => a

-- >>> :t maxBound
-- maxBound :: Bounded a => a

-- >>> maxBound
-- ()

-- >>> maxBound::Int
-- 9223372036854775807

-- >>> maxBound::Char
-- '\1114111'

-- >>> maxBound::Bool
-- True

-- >>> maxBound::Integer
-- No instance for `Bounded Integer' arising from a use of `maxBound'
-- In the expression: maxBound :: Integer
-- In an equation for `it_aJPO': it_aJPO = maxBound :: Integer

-- >>> :t (+)
-- (+) :: Num a => a -> a -> a

-- >>> :t (/)
-- (/) :: Fractional a => a -> a -> a

-- >>> :t (==)
-- (==) :: Eq a => a -> a -> Bool

-- >>> :t elem
-- elem :: (Foldable t, Eq a) => a -> t a -> Bool

-- >>> :t show
-- show :: Show a => a -> String

-- >>> :t Eq
-- Illegal term-level use of the type constructor or class `Eq'
--   imported qualified from `Prelude'
--   (and originally defined in `GHC.Classes')
-- Perhaps use `EQ' (imported from Prelude)
-- In the expression: Eq

-- >>> :k Eq
-- Eq :: * -> Constraint

class Measurable a where
    size :: a -> Integer
    empty :: a -> Bool
    empty = (==0) . size

-- >>> :k Measurable
-- Measurable :: * -> Constraint

-- >>> :t size
-- size :: Measurable a => a -> Integer

-- >>> :t empty
-- empty :: Measurable a => a -> Bool


larger :: Measurable a => a -> a -> Bool
larger x y = size x > size y

instance Measurable Integer where
    size 0 = 0
    size n = 1 + size (n `div` 10)
--    empty x = x <= 0

-- >>> size 23984248947
-- 11

-- >>> empty 213312
-- False

instance (Measurable a, Measurable b) => Measurable (a, b) where
  size (x,y) = size x + size y

-- >>> size (283474839723, 23894238947)
-- 23

instance Measurable a => Measurable [a] where
  size = foldr (\x r -> size x + r) 0

-- >>> size [1123,123,13]
-- 9

-- >>> size ([12,13], [(123,123),(12213,123321)])
-- 21

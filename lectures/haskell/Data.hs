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

data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Eq, Ord, Show, Read, Enum)

today :: Weekday
today = Wed

{-
instance Show Weekday where
  show Sat = "Sat"
  show Sun = "Sun"
  show _   = "Blah"
-}

-- >>> today
-- Wed

-- >>> read "Wed"::Weekday
-- Wed

-- >>> Mon < Thu
-- True

-- >>> [Mon .. Fri]
-- [Mon,Tue,Wed,Thu,Fri]

-- >>> :k Weekday
-- Weekday :: *

type Name = String
type Score = Int

--data Player = Player Name Score
data Player = Player { name :: Name, score :: Score }
  deriving (Eq, Ord, Read, Show)

-- >>> :t Player
-- Player :: Name -> Score -> Player

-- >>> :k Player
-- Player :: *

katniss :: Player
-- katniss = Player "Katniss Everdeen" 45
katniss = Player { score = 45, name = "Katniss Everdeen" }

-- >>> katniss == katniss
-- True

-- >>> katniss
-- Player {name = "Katniss Everdeen", score = 45}

-- >>> name katniss
-- "Katniss Everdeen"

-- >>> score katniss
-- 45

data Shape = Circle { radius :: Double } | Rectangle { width, height :: Double }
  deriving (Eq, Show, Read, Ord)

circle :: Shape
circle = Circle 2.3

rectangle :: Shape
rectangle = Rectangle 5.3 7.5

area :: Shape -> Double
area (Circle r) = pi * r * r
area (Rectangle h w) = h * w

-- >>> circle
-- Circle {radius = 2.3}

-- >>> area circle
-- 16.619025137490002

-- >>> area rectangle
-- 39.75

-- >>> circle < rectangle
-- True

x :: Maybe Int
x = Just 5

y :: Maybe Int
y = Nothing


-- >>> x
-- Just 5

-- >>> y
-- Nothing

-- >>> :t Just
-- Just :: a -> Maybe a

-- >>> :k Maybe
-- Maybe :: * -> *

-- >>> :k Either
-- Either :: * -> * -> *

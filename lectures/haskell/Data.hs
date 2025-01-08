module Data where

import Prelude hiding (Functor)

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

data Nat = Zero | Succ Nat
  deriving (Show, Read, Eq, Ord)

-- Zero < Succ Zero < Succ $ Succ Zero < Succ $ Succ $ Succ Zero

five = Succ $ Succ $ Succ $ Succ $ Succ Zero

-- >>> five
-- Succ (Succ (Succ (Succ (Succ Zero))))

fromNat :: Nat -> Integer
fromNat Zero     = 0
fromNat (Succ n) = 1 + fromNat n

-- >>> fromNat five
-- 5

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n = Succ $ toNat $ n - 1

-- >>> toNat 10
-- Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))))

plusNat :: Nat -> Nat -> Nat
plusNat Zero n = n
plusNat (Succ m) n = Succ $ plusNat m n

-- >>> fromNat $ plusNat (toNat 5) (toNat 8)
-- 13

data Bin = One | BitZero Bin | BitOne Bin
  deriving (Show, Read, Eq, Ord)

six = BitZero $ BitOne One

-- >>> six
-- BitZero (BitOne One)

fromBin :: Bin -> Integer
fromBin One          = 1
fromBin (BitZero b)  = 2 * fromBin b
fromBin (BitOne b)   = 1 + 2 * fromBin b

-- >>> fromBin six
-- 6

toBin :: Integer -> Bin
toBin 1 = One
toBin n
  | n <= 0 = error "Работи само с положителни числа" 
  | even n = BitZero $ toBin $ div n 2
  | odd n  = BitOne  $ toBin $ div n 2

-- >>> toBin 100
-- BitZero (BitZero (BitOne (BitZero (BitZero (BitOne One)))))

-- >>> fromBin $ toBin 100
-- 100

succBin :: Bin -> Bin
succBin One         = BitZero One
succBin (BitZero b) = BitOne b
succBin (BitOne  b) = BitZero $ succBin b

-- >>> fromBin $ succBin $ toBin 31
-- 32

plusBin :: Bin -> Bin -> Bin
plusBin One         c = succBin c
plusBin (BitZero b) (BitZero c) = BitZero $ plusBin b c
plusBin (BitZero b) (BitOne  c) = BitOne  $ plusBin b c
plusBin (BitOne  b) (BitZero c) = BitOne  $ plusBin b c
plusBin (BitOne  b) (BitOne  c) = BitZero $ succBin $ plusBin b c

-- >>> fromBin $ plusBin (toBin 555) (toBin 888)
-- 1443

--data List a = Nil | Cons a (List a)
data List a = Nil | Cons { listHead :: a , listTail :: List a }

-- List Bool = { Nil, Cons False Nil, Cons True Nil, Cons False ....}
  deriving (Eq, Ord, Show, Read)

l :: List Integer
l = Cons 1 $ Cons 2 $ Cons 3 Nil

l2 :: List Bin
l2 = Cons One $ Cons six $ Cons (plusBin six six) Nil

-- >>> :k List
-- List :: * -> *

-- >>> listHead l
-- 1

-- >>> listTail l2
-- Cons {listHead = BitZero (BitOne One), listTail = Cons {listHead = BitZero (BitZero (BitOne One)), listTail = Nil}}

fromList :: List a -> [a]
fromList Nil        = []
fromList (Cons x l) = x : fromList l

-- >>> fromList l
-- [1,2,3]

-- >>> fromList l2
-- [One,BitZero (BitOne One),BitZero (BitZero (BitOne One))]

-- >>> map fromBin $ fromList l2
-- [1,6,12]

toList :: [a] -> List a
{-
toList []     = Nil
toList (x:xs) = Cons x (toList xs)
-}

toList = foldr Cons Nil

(+++) :: List a -> List a -> List a
Nil +++ l = l
(Cons x xs) +++ l = Cons x $ xs +++ l

-- >>> fromList $ toList [1..3] +++ toList [5..10]  
-- [1,2,3,5,6,7,8,9,10]

data BinTree a = Empty | Node { root :: a, left, right :: BinTree a }
  deriving (Eq, Ord, Show, Read)

leaf x = Node x Empty Empty

t = Node 1 (leaf 2) (leaf 3)

-- >>> t
-- Node {root = 1, left = Node {root = 2, left = Empty, right = Empty}, right = Node {root = 3, left = Empty, right = Empty}}

depth :: BinTree a -> Integer
depth Empty         = 0
depth (Node _ l r)  = 1 + max (depth l) (depth r)

-- >>> depth t
-- 2

leaves :: BinTree a -> [a]
leaves Empty                 = []
leaves (Node x Empty Empty)  = [x]
leaves (Node x l     r)      = leaves l ++ leaves r

-- >>> leaves t
-- [2,3]

mapBinTree :: (a -> b) -> BinTree a -> BinTree b
mapBinTree _ Empty = Empty
mapBinTree f (Node x l r) = Node (f x) (mapBinTree f l) (mapBinTree f r)

-- >>> mapBinTree (+1) t
-- Node {root = 2, left = Node {root = 3, left = Empty, right = Empty}, right = Node {root = 4, left = Empty, right = Empty}}

-- TODO foldBinTree :: (a -> b -> b -> b) -> b -> BinTree a -> b

foldrBinTree :: (a -> b -> b) -> b -> BinTree a -> b
foldrBinTree _  nv Empty = nv
foldrBinTree op nv (Node x l r) = op x (foldrBinTree op (foldrBinTree op nv l) r)

-- >>> foldrBinTree (+) 0 t
-- 6

data Tree a     = Tree { rootTree :: a, subtrees :: TreeList a }
   deriving (Eq, Ord, Read, Show)
data TreeList a = None | SubTree { firstTree :: Tree a, restTrees :: TreeList a }
   deriving (Eq, Ord, Read, Show)

leafTree x = Tree x None

tree = Tree 1 $ SubTree (leafTree 2)
              $ SubTree (Tree 3 $ SubTree (leafTree 4) None)
              $ SubTree (leafTree 5) None

-- >>> tree
-- Tree {rootTree = 1, subtrees = SubTree {firstTree = Tree {rootTree = 2, subtrees = None}, restTrees = SubTree {firstTree = Tree {rootTree = 3, subtrees = SubTree {firstTree = Tree {rootTree = 4, subtrees = None}, restTrees = None}}, restTrees = SubTree {firstTree = Tree {rootTree = 5, subtrees = None}, restTrees = None}}}}

level :: Integer -> Tree a -> [a]
-- level 0 t = [rootTree t]
level 0 (Tree x _) = [x]
-- levle n t = levelTrees (n - 1) (subtrees t)
level n (Tree _ ts) = levelTrees (n - 1) ts

levelTrees :: Integer -> TreeList a -> [a]
levelTrees n None           = []
levelTrees n (SubTree t ts) = level n t ++ levelTrees n ts

-- >>> map (\n -> level n tree) [0..2] 
-- [[1],[2,3,5],[4]]

-- >>> map (`level` tree) [0..2] 
-- [[1],[2,3,5],[4]]

{-
data SExpr = SBool Bool | SChar Char | SInt Int |
             SDouble Double | SPair SExpr SExpr
-}

data SExpr = SBool Bool | SChar Char | SInt Int |
             SDouble Double | SList { list :: [SExpr] }
             deriving (Eq, Ord, Show, Read)

sexpr = SList [SInt 2, SChar 'a', SList [SBool True, SDouble 1.2, SList []]]

-- >>> sexpr
-- SList {list = [SInt 2,SChar 'a',SList {list = [SBool True,SDouble 1.2,SList {list = []}]}]}

countAtoms :: SExpr -> Integer
countAtoms (SList ses) = sum $ map countAtoms ses
countAtoms _          = 1

-- >>> countAtoms sexpr
-- 4

flatten :: SExpr -> SExpr -- всъщност е SList ...
flatten (SList ses) = SList $ concatMap (list . flatten) ses
flatten atom = SList [atom]
-- >>> flatten sexpr
-- SList {list = [SInt 2,SChar 'a',SBool True,SDouble 1.2]}



flattenH :: SExpr -> [SExpr]
flattenH (SList ses) = concatMap flattenH ses
flattenH atom = [atom]

-- >>> flattenH sexpr
-- [SInt 2,SChar 'a',SBool True,SDouble 1.2]

class Countable c where
  count :: c a -> Int

-- type HaskellList a = [a]

-- >>> :t [True]
-- [True] :: [Bool]

-- >>> :k [Bool]
-- [Bool] :: *

--- >>> :t []
-- [] :: [a]

-- >>> :k []
-- [] :: * -> *

-- >>> :k ([] Bool)
-- ([] Bool) :: *

instance Countable [] where
  count = length

class Listable c where
  elements :: c a -> [a]

instance Listable [] where
  elements = id


class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap = map

instance Functor BinTree where
  fmap = mapBinTree


module Functor where

import Prelude hiding (Functor, fmap, (<$>), Applicative, pure, (<*>))
import Data (mapBinTree)
import Main


class Functor f where
  fmap , (<$>):: (a -> b) -> f a -> f b
  (<$>) = fmap

instance Functor [] where
  fmap = map

-- >>> map (+1) [1..5]
-- [2,3,4,5,6]

-- >>> (+1) <$> [1..5]
-- [2,3,4,5,6]

instance Functor Maybe where
   fmap :: (a -> b) -> Maybe a -> Maybe b
   fmap _ Nothing  = Nothing
   fmap f (Just x) = Just (f x)

--- >>> (+1) <$> Just 2
-- Just 3

-- cons h <$> t

-- >>> :t (,)
-- (,) :: a -> b -> (a, b)

-- >>> :k (,)
-- (,) :: * -> * -> *

-- >>> :k (,) Int
-- (,) Int :: * -> *

instance Functor ((,) a) where
    -- fmap :: (b -> c) -> (((,) a) b) -> (((,) a) c)
    fmap :: (b -> c) -> (a, b) -> (a, c)
    fmap f (x, y) = (x, f y)

-- >>> (+1) <$> (4, 5)
-- (4,6)

newtype FlippedPair a b = FlippedPair (b, a)
  deriving (Read, Show, Eq, Ord)
  
instance Functor (FlippedPair a) where
    fmap :: (b -> c) -> FlippedPair a b -> FlippedPair a c
    fmap f (FlippedPair (y, x)) = FlippedPair (f y, x)

-- >>> (+1) <$> FlippedPair (4, 7)
-- FlippedPair (5,7)

instance Functor (Either a) where
    fmap :: (b -> c) -> Either a b -> Either a c
    fmap f (Left x) = Left x
    fmap f (Right y) = Right (f y)

-- >>> (+1) <$> Left 3
-- Left 3

-- >>> (+1) <$> Right 3
-- Right 4

-- >>> :k (->)
-- (->) :: * -> * -> *

-- >>> :k (Int -> Bool)
-- (Int -> Bool) :: *

-- >>> :k (->) Int Bool
-- (->) Int Bool :: *

-- >>> :k (->) Int
-- (->) Int :: * -> *

instance Functor ((->) r) where
    -- fmap :: (a -> b) -> (((->) r) a) -> (((->) r) b)
    fmap :: (a -> b) -> (r -> a) -> r -> b
    -- fmap f g x = f (g x)
    -- fmap f g = f . g
    fmap = (.)

-- >>> ((+1) <$> (*2)) 7
-- 15

instance Functor IO where
    fmap :: (a -> b) -> IO a -> IO b
    {-
    fmap f t = do x <- t
                  return $ f x
    -}
    -- fmap f t = t >>= (return . f)
    fmap f = (return . f =<<)

-- >>> ((+) $ 2) $ 3
-- 5

-- >>> ((+) <$> Just 2) <$> Just 3
-- Couldn't match expected type: a1_apnpp[tau:1] -> b_apnpq[sk:1]
--             with actual type: Maybe (a0_apnpu[tau:1] -> a0_apnpu[tau:1])
-- Possible cause: `(<$>)' is applied to too many arguments
-- In the first argument of `(<$>)', namely `((+) <$> Just 2)'
-- In the expression: ((+) <$> Just 2) <$> Just 3
-- In an equation for `it_apnnq':
--     it_apnnq = ((+) <$> Just 2) <$> Just 3
-- Relevant bindings include
--   it_apnnq :: Maybe b_apnpq[sk:1]
--     (bound at /home/trifon/fmisync/Courses/2024_25/FP_2024_25/fp-2024-25/lectures/haskell/Functor.hs:102:2)

class Functor f => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
    -- fmap f x = pure f <*> x
    -- fmap f = (pure f <*>)
    -- fmap f = (<*>) (pure f)
    -- fmap = (<*>) . pure

instance Applicative Maybe where
    pure :: a -> Maybe a
    pure = Just
    (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    Nothing <*> _       = Nothing
    _       <*> Nothing = Nothing
    Just f <*> Just x   = Just (f x)

-- >>> (+) <$> Just 3 <*> Just 5
-- Just 8

-- >>> pure (+) <*> Just 3 <*> Just 5
-- Just 8

instance Applicative (Either a) where
    pure :: b -> Either a b
    pure = Right
    (<*>) :: Either a (b -> c) -> Either a b -> Either a c
    Left x <*>  _      = Left x
    Right f <*> Left x = Left x
    Right f <*> Right y = Right (f y)


-- Either String Int
-- >>> (+) <$> Left "Error" <*> Right 8
-- Left "Error"

-- >>> (+) <$> Right 3 <*> Left "Error"
-- Left "Error"

-- >>> (+) <$> Right 3 <*> Right 5
-- Right 8

-- >>> (+) <$> Left "Error1" <*> Left "Error2"
-- Left "Error1"

instance Applicative [] where
    pure :: a -> [a]
    -- pure x = [x]
    pure = (:[])
    (<*>) :: [a -> b] -> [a] -> [b]
    fs <*> xs = [ f x | f <- fs, x <- xs ]

-- >>> (+) <$> [1, 2, 3] <*> [4, 5, 6]
-- [5,6,7,6,7,8,7,8,9]

newtype ZipList a = ZipList [a]
  deriving (Eq, Ord, Read, Show)

instance Functor ZipList where
    fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap f (ZipList xs)= ZipList $ map f xs 

instance Applicative ZipList where
    -- pure x = ZipList [x]
    pure = ZipList . (:[])
    ZipList fs <*> ZipList xs = ZipList $ zipWith ($) fs xs

-- >>> (+) <$> ZipList [1, 2, 3] <*> ZipList [4, 5, 6]
-- ZipList [5,7,9]

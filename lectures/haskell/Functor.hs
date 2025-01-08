module Functor where

import Prelude hiding (Functor, fmap, (<$>))
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


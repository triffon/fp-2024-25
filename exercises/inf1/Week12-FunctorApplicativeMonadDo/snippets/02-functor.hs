main :: IO()
main = do
    print $ fmap (+3) (Box 5)
    print $ fmap (+3) (Just 5)
    print $ fmap (+3) Nothing
    let either1 = Right 5 :: Either Int Int
    let either2 = Left 5 :: Either Int Int
    print $ fmap (+3) either1
    print $ fmap (+3) either2

data Box a = Box a 
    deriving (Show)

instance Functor Box where
    -- fmap :: Functor f => (a -> b) -> f a -> f b
    fmap f (Box x) = Box (f x)
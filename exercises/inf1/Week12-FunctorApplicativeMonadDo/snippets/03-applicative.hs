main :: IO()
main = do
    print $ (pure 5 :: Box Int)
    print $ Box (+3) <*> Box 5

    print $ (pure 5 :: Maybe Int)
    print $ Just (+3) <*> Just 5
    let nothing = Nothing :: Maybe (Int -> Int)
    print $ nothing <*> Just 5

    print $ (pure 5 :: Either String Int)
    let right1 = Right (+3) :: Either String (Int -> Int)
    let right2 = Right 5 :: Either String Int
    print $ right1 <*> right2
    let left1 = Left "error1" :: Either String (Int -> Int)
    let left2 = Left "error2" :: Either String Int
    print $ left1 <*> left2
    print $ right1 <*> left2
    print $ left1 <*> right2

data Box a = Box a 
    deriving (Show)

instance Functor Box where
    -- fmap :: Functor f => (a -> b) -> f a -> f b
    fmap f (Box x) = Box (f x)

instance Applicative Box where
    -- pure :: Applicative f => a -> f a
    pure a = Box a
    -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
    (Box f) <*> (Box x) = Box (f x) 
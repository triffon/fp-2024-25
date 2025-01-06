main :: IO()
main = do
    print $ (return 5 :: Box Int)
    print $ Box 5 >>= (\ x -> Box (x + 3))

    print $ (return 5 :: Maybe Int)
    print $ Just 5 >>= (\ x -> Just (x + 3))
    print $ (Nothing :: Maybe Int) >>= (\ x -> Just (x + 3))
    print $ Just 5 >>= (\ x -> (Nothing :: Maybe Int))
    
    print $ (return 5 :: Either String Int)
    let right = Right 5 :: Either String Int
    let left1 = Left "error1" :: Either String Int
    let left2 = Left "error2" :: Either String Int
    print $ right >>= (\ x -> (Right (x + 3) :: Either String Int))
    print $ left1 >>= (\ x -> (Right (x + 3) :: Either String Int))
    print $ left2 >>= (\ x -> left1)
    print $ right >>= (\ x -> left1)

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

instance Monad Box where
    -- return :: Monad m => a -> m a
    return = pure
    -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
    (Box x) >>= f = f x 
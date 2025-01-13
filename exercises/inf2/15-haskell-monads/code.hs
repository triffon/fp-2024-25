{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors


data Perhaps a 
    = Yes a 
    | Nope
    deriving (Eq, Show)

-- instance Eq a => Eq (Perhaps a) where
--     Yes x == Yes y = x == y
--     Nope == Nope = True
--     _ == _ = False

safeHead :: [a] -> Perhaps a
safeHead (x:_) = Yes x
safeHead [] = Nope

recip' :: Float -> Perhaps Float
recip' x
    | x /= 0 = Yes (1.0 / x)
    | otherwise = Nope

foo :: Float -> Float -> Perhaps Float
foo a b
    | (Yes ar) <- recip' a, (Yes br) <- recip' b = Yes (ar + br)
    | otherwise = Nope

pMap :: (a -> b) -> Perhaps a -> Perhaps b
pMap f (Yes x) = Yes (f x)
pMap _ Nope = Nope

instance Functor Perhaps where
    -- fmap :: (a -> b) -> Perhaps a -> Perhaps b
    fmap f (Yes x) = Yes (f x)
    fmap _ Nope = Nope

instance Applicative Perhaps where
    -- (<*>) :: Perhaps (a -> b) -> Perhaps a -> Perhaps b
    (<*>) (Yes f) (Yes x) = Yes (f x)
    (<*>) _ _ = Nope
    pure = Yes

instance Monad Perhaps where
    -- (>>=) :: Perhaps a -> (a -> Perhaps b) -> Perhaps b
    (>>=) (Yes x) f = f x
    (>>=) Nope _ = Nope

foo' :: Float -> Float -> Perhaps Float
foo' a b = (recip' a) >>= (\ar -> (recip' b) >>= (\br -> Yes (ar + br)))

foo'' :: Float -> Float -> Perhaps Float
foo'' a b = do
    ar <- recip' a
    br <- recip' b
    pure (ar + br)

data State s a = WithState (s -> (a, s))

instance Functor (State s) where
    -- fmap :: (a -> b) -> (State s a) -> (State s b)
    fmap f (WithState run) = WithState run'
        where
            -- run' :: (s -> (b, s))
            run' beginState = (f x, prevState)
                where
                    (x, prevState) = run beginState

instance Applicative (State s) where
    pure x = WithState (\beginState -> (x, beginState))
    (WithState runF) <*> (WithState runX) = WithState run
        where
            -- run :: s -> (b, s)
            run beginState = (f x, prevState2)
                where
                    (f, prevState) = runF beginState
                    (x, prevState2) = runX prevState

instance Monad (State s) where
    -- f :: a -> State s b
    (WithState runX) >>= f = WithState run
        where
            -- run :: s -> (b s)
            run beginState = final
                where
                    (x, prevState) = runX beginState
                    (WithState runFX) = f x
                    final@(_y, _finalState) = runFX prevState

getState :: State s s
getState = WithState run
    where
        run internalState = (internalState, internalState)

setState :: s -> (State s ())
setState newState = WithState run
    where
        run _prevState = ((), newState)

runState :: s -> State s a -> a
runState beginState (WithState run) = x
    where
        (x, _finalState) = run beginState

data Tree = Tree
    { children :: [Tree]
    , label :: String
    }
    deriving (Show)

myTree :: Tree
myTree = Tree
    { label = "root"
    , children = [
        Tree
            { label = "foo"
            , children = []
            },
        Tree
            { label = "bar"
            , children = [
                Tree
                    { label = "qux"
                    , children = []
                    },
                Tree
                    { label = "qox"
                    , children = []
                    }
                ]
            }
        ]
    }

walkTree' :: Tree -> State Int Tree
walkTree' (Tree { label, children }) = do
    counter <- getState
    setState (counter + 1)

    newChildren <- mapM walkTree' children

    let result = Tree
                    { label = label ++ (show counter)
                    , children = newChildren }

    pure result

walkTree :: Tree -> Tree
walkTree = (runState 0) . walkTree'


hello :: IO ()
hello = putStrLn "hello world"

greet :: String -> IO ()
greet s = do
    putStr "your name: "
    name <- getLine
    putStrLn (s ++ ", " ++ name)

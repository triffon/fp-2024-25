{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors

data Perhaps a
    = Yes a
    | Nope
    deriving (Show, Eq)

safeHead :: [a] -> Perhaps a
safeHead (x:_) = Yes x
safeHead [] = Nope

sumFirst :: [[Int]] -> Perhaps Int
sumFirst (l:ls)
    | (Yes hl) <- safeHead l, (Yes osum) <- sumFirst ls
        = Yes $ hl + osum
    | otherwise = Nope
sumFirst [] = Yes 0

headPlusOne :: [Int] -> Perhaps Int
headPlusOne l
    | (Yes x) <- safeHead l = Yes (x + 1)
    | otherwise = Nope

instance Functor Perhaps where
    -- fmap :: (a -> b) -> Perhaps a -> Perhaps b
    fmap f (Yes x) = Yes (f x)
    fmap _ Nope = Nope

instance Applicative Perhaps where
    -- pure :: a -> Perhaps a
    pure = Yes

    -- (<*>) :: Perhaps (a -> b) -> Perhaps a -> Perhaps b
    (Yes f) <*> (Yes x) = Yes (f x)
    _ <*> _ = Nope

instance Monad Perhaps where
    -- (>>=) :: Perhaps a -> (a -> Perhaps b) -> Perhaps b
    (Yes x) >>= f = f x
    Nope >>= _ = Nope

headPlusOne' :: [Int] -> Perhaps Int
headPlusOne' l = fmap (1+) (safeHead l)

sumFirst' :: [[Int]] -> Perhaps Int
sumFirst' (l:ls)
    = (safeHead l) >>=
        (\hl -> (sumFirst' ls) >>=
            (\osum -> Yes (hl + osum)))
sumFirst' [] = Yes 0

sumFirst'' :: [[Int]] -> Perhaps Int
sumFirst'' (l:ls) = do
    hl <- safeHead l
    osum <- sumFirst'' ls
    Yes (hl + osum)
sumFirst'' [] = Yes 0

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

data BinTree
    = Node String BinTree BinTree
    | Leaf
    deriving (Show)

myBinTree :: BinTree
myBinTree = Node "root"
    (Node "foo" Leaf Leaf)
    (Node "bar"
        (Node "qux" Leaf Leaf)
        (Node "qox" Leaf Leaf))

numBinTree' :: Int -> BinTree -> (BinTree, Int)
numBinTree' n (Node label left right)
    = (Node (label ++ (show n)) leftTree rightTree, nR)
    where
        (leftTree, nL) = numBinTree' (n+1) left
        (rightTree, nR) = numBinTree' (nL) right
numBinTree' n Leaf = (Leaf, n)

data State s a = WithState (s -> (a, s))

instance Functor (State s) where
    -- fmap :: (a -> b) -> State s a -> State s b
    fmap f (WithState run) = WithState run'
        where
            -- run' :: s -> (b, s)
            run' beginState = (f x, prevState)
                where
                    (x, prevState) = run beginState

instance Applicative (State s) where
    -- pure :: a -> State s a
    pure x = WithState (\beginState -> (x, beginState))

    -- (<*>) :: State s (a -> b) -> State s a -> State s b
    (WithState runF) <*> (WithState runX) = WithState run
        where
            run beginState = (f x, finalState)
                where
                    (f, stateAfterF) = runF beginState
                    (x, finalState) = runX stateAfterF

instance Monad (State s) where
    -- (>>=) :: State s a -> (a -> State s b) -> State s b
    (WithState runX) >>= f = WithState run
        where
            run beginState = (y, finalState)
                where
                    (x, stateAfterX) = runX beginState
                    (WithState runY) = f x
                    (y, finalState) = runY stateAfterX

getState :: State s s
getState = WithState run
    where
        run beginState = (beginState, beginState)

setState :: s -> State s ()
setState newState = WithState run
    where
        run _beginState = ((), newState)

runState :: s -> State s a -> a
runState beginState (WithState run) = x
    where
        (x, _finalState) = run beginState

numBinTree'' :: BinTree -> State Int BinTree
numBinTree'' (Node label left right) = do
    n <- getState
    let newLabel = label ++ (show n)
    setState (n + 1)

    leftRes <- numBinTree'' left
    rightRes <- numBinTree'' right

    pure (Node newLabel leftRes rightRes)
numBinTree'' Leaf = pure Leaf

numTree' :: Tree -> State Int Tree
numTree' (Tree { children, label }) = do
    n <- getState
    let newLabel = label ++ (show n)
    setState (n + 1)

    newChildren <- myMapM numTree' children

    pure (Tree { children = newChildren, label = newLabel })

myMapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
myMapM f (x:xs) = do
    y <- f x
    ys <- myMapM f xs
    pure (y:ys)
myMapM _ [] = pure []

numTree :: Tree -> Tree
numTree = (runState 1) . numTree'

hello :: String -> IO ()
hello name = putStrLn ("hello, " ++ name)

helloMulti :: [String] -> IO ()
helloMulti names = do
    _ <- myMapM hello names
    pure ()

import Data.Char

main :: IO()
main = do
    print $ numberBTree
    print $ charBTree

    print $ size numberBTree == 9
    print $ size charBTree == 7

    print $ sumTree numberBTree == 146
    -- print $ sumTree charBTree -- should not work

    print $ traverseDFS numberBTree == [96, 1, 12, 0, 5, 2, 4, 5, 21]
    print $ traverseDFS charBTree == "haskell"

    print $ getLevel numberBTree 2 == [1, 0, 2, 5]
    print $ getLevel charBTree 1 == "al"
    print $ getLevel charBTree 3 == []

    print $ traverseBFS numberBTree == [5,12,4,1,0,2,5,96,21]
    print $ traverseBFS charBTree == "kalhsel"

    print $ mapTree numberBTree (*2) == Node 10 (Node 24 (Node 2 (Node 192 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 8 (Node 4 Nil Nil) (Node 10 Nil (Node 42 Nil Nil)))
    print $ mapTree charBTree (toUpper) == Node 'K' (Node 'A' (Node 'H' Nil Nil) (Node 'S' Nil Nil)) (Node 'L' (Node 'E' Nil Nil) (Node 'L' Nil Nil))

    print $ mirrorTree numberBTree == Node 5 (Node 4 (Node 5 (Node 21 Nil Nil) Nil) (Node 2 Nil Nil)) (Node 12 (Node 0 Nil Nil) (Node 1 Nil (Node 96 Nil Nil)))
    print $ mirrorTree charBTree == Node 'k' (Node 'l' (Node 'l' Nil Nil) (Node 'e' Nil Nil)) (Node 'a' (Node 's' Nil Nil) (Node 'h' Nil Nil))

data BTree a = Nil | Node a (BTree a) (BTree a)
    deriving (Show, Eq)

numberBTree :: BTree Int
numberBTree = Node 5 
                (Node 12 
                    (Node 1 
                        (Node 96 Nil Nil) 
                        Nil) 
                    (Node 0 Nil Nil))
                (Node 4 
                    (Node 2 Nil Nil)
                    (Node 5 Nil (Node 21 Nil Nil)))

charBTree :: BTree Char
charBTree = Node 'k' 
                (Node 'a' (Node 'h' Nil Nil) (Node 's' Nil Nil))
                (Node 'l' (Node 'e' Nil Nil) (Node 'l' Nil Nil))

size :: BTree a -> Int
size Nil = 0
size (Node _ st1 st2) = 1 + size st1 + size st2

sumTree :: (Num a) => BTree a -> a
sumTree Nil = 0
sumTree (Node d st1 st2) = d + sumTree st1 + sumTree st2

traverseDFS :: BTree a -> [a]
traverseDFS Nil = []
traverseDFS (Node x left right) = traverseDFS left ++ [x] ++ traverseDFS right

getLevel :: BTree a -> Int -> [a]
getLevel Nil _ = []
getLevel (Node x _ _) 0 = [x]
getLevel (Node _ left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)

traverseBFS :: (Eq a) => BTree a -> [a]
traverseBFS t = concat $ takeWhile (/= []) $ map (getLevel t) [0 ..]

mapTree :: BTree a -> (a -> b) -> BTree b
mapTree Nil _ = Nil
mapTree (Node x left right) f = (Node (f x) (mapTree left f) (mapTree right f))

mirrorTree :: BTree a -> BTree a
mirrorTree Nil = Nil
mirrorTree (Node x left right) = (Node x (mirrorTree right) (mirrorTree left))
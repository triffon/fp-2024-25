main :: IO()
main = do
    print $ colourBTree
    print $ highest Red colourBTree
    print $ highest Green colourBTree
    print $ highest Blue colourBTree
    print $ highest Red colourBTree == 4
    print $ highest Green colourBTree == 3
    print $ highest Blue colourBTree == 4

data BTree a = Nil | Node a (BTree a) (BTree a)
    deriving (Show, Eq)

data Color = Blue | Green | Red
 deriving (Show, Eq)

colourBTree = Node Blue 
                (Node Green 
                    (Node Blue 
                        (Node Red Nil Nil) 
                        Nil) 
                    (Node Blue 
                        Nil 
                        Nil))
                (Node Red
                    (Node Green
                        (Node Blue Nil Nil)
                        Nil)
                    (Node Red
                        Nil
                        Nil))

getLevel :: BTree a -> Int -> [a]
getLevel Nil _ = []
getLevel (Node x _ _) 0 = [x]
getLevel (Node _ left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)

highest :: Color -> BTree Color -> Int
highest s n@(Node colorL left right) = helper s n 0
    where
        helper :: Color -> BTree Color -> Int -> Int
        helper _ Nil currentMax = currentMax
        helper colorS (Node colorL left right) currentMax
         | colorS == colorL = max (helper colorS left (currentMax + 1)) (helper colorS right (currentMax + 1))
         | otherwise = max (helper colorS left currentMax) (helper colorS right currentMax)
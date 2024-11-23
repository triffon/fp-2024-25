import Prelude hiding(length, map, zipWith, foldr, foldl, reverse)

length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ nv [] = nv
foldr op nv (x:xs) = op x (foldr op nv xs)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ nv [] = nv
foldl op nv (x:xs) = foldl op (op nv x) xs

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf [] _ = True
isInfixOf _ [] = False
isInfixOf l r@(y:ys) = prefixList l r || isInfixOf l ys
  where prefixList :: Eq a => [a] -> [a] -> Bool
        prefixList [] _ = True
        prefixList _ [] = False
        prefixList (x:xs) (y:ys) = x == y && prefixList xs ys

maximumBy :: (a -> a -> Bool) -> [a] -> a
maximumBy cmp = foldr1 (\curr result -> if cmp curr result then curr else result)

removeConsequtive :: Eq a => [a] -> [a]
removeConsequtive [] = []
removeConsequtive (x:xs) = x : removeConsequtive (dropWhile (== x) xs)

pairSum :: Int -> [Int] -> [(Int, Int)]
pairSum n lst = [(x, y) | x <- lst, y <- lst, x + y == n, x <= y]

leftPad :: [a] -> Int -> a -> [a]
leftPad lst n x = replicate (n - length lst) x ++ lst

quickSortBy :: (a -> a -> Bool) -> [a] -> [a]
quickSortBy _ [] = []
quickSortBy cmp (x:xs) = let lhs = filter (`cmp` x) xs
                             rhs = filter (x `cmp`) xs
  in quickSortBy cmp lhs ++ [x] ++ quickSortBy cmp rhs


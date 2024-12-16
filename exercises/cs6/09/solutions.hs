import Prelude hiding (concatMap, foldl, splitAt, takeWhile, zip, zipWith)

-- Типовете на елементите на списъка и акумулатора са различни
-- както ако в Scheme правите foldl върху списък от числа,
-- но с предикат и логическа връзка (and, or) натрупвате булева стойност
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc [] = acc
foldl op acc (x : xs) = foldl op (op acc x) xs

-- За дадени два списъка [a1, ..., an] и [b1, ..., bn],
-- връща списъка от наредени двойки [(a1,b1), ..., (an,bn)]
-- Да се покрие и случаят, в който списъците имат различна дължина
zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys

-- Прилага поелементно функция върху двата списъка едновременно.
-- Връща списък от резултатите. Като map на 2 списъка в scheme
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x : xs) (y : ys) = f x y : zipWith f xs ys

-- Връща най-големия префикс на списък,
-- такъв че даденият предикат е изпълнен за всичките му елементи
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x : xs) = if p x then x : takeWhile p xs else []

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap _ [] = []
concatMap f (x : xs) = f x ++ concatMap f xs

-- за списък от цели числа, премахва дубликатите,
-- т.е. запазва само първите срещания на даден елемент
nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x : xs) = x : nub (filter (/= x) xs)

-- Проверява дали число е просто
prime :: Int -> Bool
prime n
  | n < 2 = False
  | otherwise = null [x | x <- [2 .. n - 1], n `rem` x == 0]

-- За дадено число n връща списък от първите n прости (положителни) числа
primes :: Int -> [Int]
primes n = take n $ filter prime [2 ..]

-- За дадено естествено число, връща списък от простите му делители
-- factorize 60 = [2, 2, 3, 5]
-- 2*2*3*5 = 60
factorize :: Int -> [Int]
factorize 1 = []
factorize n =
  let (d : ds) = [x | x <- [2 .. n], prime x, n `rem` x == 0]
   in d : factorize (n `div` d)

-- quicksort за цели числа
-- Няма нужда да взимаме случаен елемент
-- просто можем да вземем първия
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) = lower ++ x : higher
  where
    lower = quicksort [y | y <- xs, y <= x]
    higher = quicksort [y | y <- xs, y > x]

------- MORE PROBLEMS

-- 2. Subsequences
subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x : xs) = subs ++ map (x :) subs
  where
    subs = subsequences xs

-- 3. Permutations
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x : xs) = concatMap (insertEverywhere x) (permutations xs)
  where
    insertEverywhere y [] = [[y]]
    insertEverywhere y (z : zs) = (y : z : zs) : map (z :) (insertEverywhere y zs)

-- 4. Run-Length Encoding
encode :: (Eq a) => [a] -> [(a, Int)]
encode [] = []
encode (x : xs) = (x, length (takeWhile (== x) (x : xs))) : encode (dropWhile (== x) xs)

-- 6. Split a List
splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

-- 8. Rotate a List
shift :: Int -> [a] -> [a]
shift n xs = let len = length xs in take len (drop (n `mod` len) (cycle xs))

-- 9. Longest Prefix
longestPrefix :: (a -> Bool) -> [a] -> [a]
longestPrefix _ [] = []
longestPrefix p (x : xs)
  | p x = x : longestPrefix p xs
  | otherwise = []

-- 10. Insertion Sort
insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x : xs) = insert x (insertionSort xs)
  where
    insert y [] = [y]
    insert y (z : zs)
      | y <= z = y : z : zs
      | otherwise = z : insert y zs

-- 11. Pascal's Triangle
pascalsTriangle :: Int -> [[Int]]
pascalsTriangle 0 = []
pascalsTriangle 1 = [[1]]
pascalsTriangle n = prev ++ [zipWith (+) (0 : last prev) (last prev ++ [0])]
  where
    prev = pascalsTriangle (n - 1)

-- 12. Zig-Zag Conversion
zigZag :: [a] -> [a]
zigZag xs = concat $ zipWith (\a b -> [a, b]) firstHalf secondHalf
  where
    (firstHalf, secondHalf) = splitAt (length xs `div` 2) xs

-- (a,b,c) е Питагорова тройка ако: a*a + b*b = c*c
-- Да се напише функция която генерира безкраен списък
-- от всички Питагорови тройки.
pythagoreanTriples :: [(Int, Int, Int)]
pythagoreanTriples =
  [(a, b, c) | a <- [1 ..], b <- [1 .. a], c <- [1 .. a], p a b c]
  where
    p x y z = x ^ 2 == y ^ 2 + z ^ 2

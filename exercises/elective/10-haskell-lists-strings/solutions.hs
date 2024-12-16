import Data.Char
import Data.List
import Data.ByteString (count)
import Control.Monad (when)

-- 1.
whisper :: String -> String
whisper = map toLower

-- 2.
removeSpaces :: String -> String
removeSpaces = filter (not . isSpace)

-- 3.
switchCaps :: String -> String
switchCaps str = [if isLower c then toUpper c else toLower c | c <- str]

-- 4.
encryptChar :: Int -> Char -> Char
encryptChar n c
    | isUpper c = chr (base + offset)
    | otherwise = error "Only capital letters are supported"
    where
        base = ord 'A'
        offset = (ord c - base + n) `mod` 26

encrypt :: Int -> String -> String
encrypt n = map (encryptChar n)

decrypt :: Int -> String -> String
decrypt n = encrypt (-n)

-- 5.
joinWords :: Char -> [String] -> String
joinWords _ [] = ""
joinWords c strs = tail (concat [c:s | s <- strs])

-- 6.
rotate :: Int -> [a] -> [a]
rotate i str = b ++ a where (a, b) = splitAt i str

bwt :: String -> String
bwt str = map last (sort (rotations str))
  where
    rotations str = [rotate i str | i <- [0 .. length str - 1]]

-- 7.
indices :: (Eq a) => a -> [a] -> [Int]
indices x ys = [i | (i, y) <- zip [0 ..] ys, y == x]

-- 8.
lastIndex :: (Eq a) => a -> [a] -> Int
lastIndex x ys = last (indices x ys)

-- 9.
countMin :: Ord a => [a] -> Int
countMin xs = length [x | x <- xs, x == m]
    where m = minimum xs

-- 10.
-- Помощна функция дали едно число е просто
isPrime :: (Integral t) => t -> Bool
isPrime n = n > 1 && null [i | i <- [2 .. floor . sqrt $ fromIntegral n], n `mod` i == 0]

primeReorder :: [a] -> [a]
primeReorder xs = map snd (primes ++ nonPrimes)
    where (primes, nonPrimes) = partition (\(i, _) -> isPrime i) $ zip [2 ..] xs

-- 11.
dedup :: Eq a => [a] -> [a]
dedup [] = []
dedup (x:xs) = x : dedup (filter (/= x) xs)

-- 12.
-- a)
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge as@(x:xs) bs@(y:ys)
  | x <= y    = x : merge xs bs
  | otherwise = y : merge as ys

-- b)
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort left) (mergesort right)
    where (left, right) = splitAt (length xs `div` 2) xs

-- 13.
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = map (x:) subs ++ subs
    where subs = subsets xs

-- 14.
pick :: Int -> [a] -> [[a]]
pick 0 _ = [[]]
pick _ [] = []
pick k (x:xs) = map (x:) (pick (k-1) xs) ++ pick k xs

-- 15.
maximize :: (Ord b) => [(a -> b)] -> (a -> b)
maximize fs x = maximum [f x | f <- fs]

-- 16.
compose :: [a -> a] -> a -> a
compose = foldr (.) id

-- 17.
-- Решение със `zipWith` (подобно на редицата на Фибоначи)
facts :: (Integral t) => [t]
facts = 1:zipWith (*) [1 ..] facts

-- Решение със `scanl` (`foldl`, който си пази "историята")
facts' :: (Integral t) => [t]
facts' = scanl (*) 1 [1 ..]

-- 18.
points :: (Integral t) => [(t, t)]
points = [(x, n - x) | n <- [0 ..], x <- [0 .. n]]

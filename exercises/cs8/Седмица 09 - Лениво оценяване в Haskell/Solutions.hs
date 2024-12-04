subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = let rest = subsets xs
  in map (x:) rest ++ rest

subsets' :: Eq a => [a] -> [[a]]
subsets' lst = []:[x:xs | x <- lst, xs <- subsets' (tail (dropWhile (/= x) lst))]

hailstone :: Int -> [Int]
hailstone 1 = repeat 1
hailstone n
  | even n = n : hailstone (n `div` 2)
  | odd n = n : hailstone (3 * n + 1)


hailstone' :: Int -> [Int]
hailstone' n = iterate next n
  where next :: Int -> Int
        next 1 = 1
        next n
          | even n = n `div` 2
          | otherwise = 3 * n + 1


rationals :: [(Int, Int)]
rationals = [(x - y, y) | x <- [0..], y <- [0..x]]

pythagoreanTriples :: [(Int, Int, Int)]
pythagoreanTriples = [(x, y, z) | z <- [1..], y <- [1..z], x <- [1..y], x^2 + y^2 == z^2]

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (dropWhile (== x) xs)

generateExponents :: Int -> Int -> [Int]
generateExponents k l = nub [n | n <- [1..], x <- [1..n], y <- [1..n], x^k * y^l == n]

permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations lst = [x:xs | x <- lst, xs <- permutations (filter (/= x) lst)]

generateSumLists :: Int -> Int -> [[Int]]
generateSumLists 1 s = [[s]]
generateSumLists k s = [x:xs | x <- [0..s], xs <- generateSumLists (k - 1) (s - x)]
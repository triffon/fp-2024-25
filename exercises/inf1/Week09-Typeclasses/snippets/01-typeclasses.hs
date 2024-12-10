f :: (Num a) => a -> a
f x = x + 5

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort lower ++ [x] ++ quicksort higher
  where
    lower = [y | y <- xs, y <= x]
    higher = [y | y <- xs, y > x]


-- if only numbers, then Num
-- if ==, /=, then Eq
-- if <, <=, >=, >, then Ord
-- if only Float and Double, then Fractional
-- if only Int and Integer, then Integral
-- if making an algebraic type that has to be printed, then Show
f :: Int -> Int
f x = if x > 5 then 2 * x else x

g :: Int -> Int
g x
 | x > 5 = 2 * x
 | x > 2 = 3 * x
 | otherwise = x
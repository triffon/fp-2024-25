-- 1.
myAbs :: (Ord a, Num a) => a -> a
myAbs x = if x >= 0 then x else -x


-- 2.
isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c = a + b > c && a + c > b && b + c > a


-- 3.
countDays :: Int -> Int -> Int -> Int
countDays day month year = sum (take (month - 1) daysInMonths) + day
    where
        isLeapYear y = (y `mod` 4 == 0 && y `mod` 100 /= 0) || (y `mod` 400 == 0)
        daysInMonths = [31, if isLeapYear year then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]


-- 4.
isPrime :: Integer -> Bool
isPrime n = (n > 1) && iter 2 where
    iter i
        | i * i > n      = True
        | n `mod` i == 0 = False
        | otherwise      = iter (i + 1)

-- Решение със списък
isPrime' :: Int -> Bool
isPrime' n
    | n < 2     = False
    | otherwise = null [x | x <- [2 .. floor (sqrt (fromIntegral n))], n `mod` x == 0]


-- 5.
sumDivisors :: Integer -> Integer
sumDivisors n = iter 1 where
    iter i
        | i >= n    = 0
        | otherwise = (if n `mod` i == 0 then i else 0) + iter (i + 1)


-- 6.
isPerfect :: Integer -> Bool
isPerfect n = sumDivisors n == n


-- 7.
countBinaryDigits :: Int -> Int
countBinaryDigits n
    | n < 0     = countBinaryDigits (-n)
    | n == 0    = 1
    | otherwise = 1 + countBinaryDigits (n `div` 2)


-- 8.
countOnes :: Int -> Int
countOnes n
    | n < 0     = countOnes (-n)
    | n == 0    = 0
    | otherwise = (n `mod` 2) + countOnes (n `div` 2)

isEvil :: Int -> Bool
isEvil n = even (countOnes n)


-- 9.
sumEvil :: Int -> Int -> Int
sumEvil a b
    | a > b = 0
    | isEvil a = a + sumEvil (a + 1) b
    | otherwise = sumEvil (a + 1) b


-- 10.
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

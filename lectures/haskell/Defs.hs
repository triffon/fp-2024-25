fact n
 | n == 0 = one
 | n > 0  = n * fact (n - one)
 | otherwise = error "отрицателно число!"
 where one = 1

-- >>> fact 5
-- 120

-- >>> fact (-3)
-- отрицателно число!
-- /home/trifon/fmisync/Courses/2024_25/FP_2024_25/fp-2024-25/lectures/haskell/Defs.hs:(1,1)-(3,28): Non-exhaustive patterns in function fact

-- >>> :t error
-- error :: HasCallStack => [Char] -> a

x :: t
x = x

-- !!! >>> x
-- ... 

-- >>> (let x = 2 in x + 2) * 5
-- 20

pow2 0 _ = 1
pow2 n _ = 2 * pow2 (n - 1) 10000

-- >>> pow2 5 129482394832498
-- 32

1 !? _ = 100
x !? 2 = 200 + x
--  x !? x = x * x
x !? y = x + y + 5

-- >>> 1 !? 10
-- 100

-- >>> 3 !? 2
-- 203

-- >>> 5 !? 8
-- 18

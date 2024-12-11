module Lazy where


-- >>> x
-- Грешка

-- >>> fst x
-- 7
x = (2 + 5, error "Грешка")

-- fst (x,_) = x

f x = f x

l = 1:2:f 0:[]

-- >>> head l
-- 1

-- >>> take 2 l
-- [1,2]

-- head (x:_) = x

g (x, y) = x + 2

h x = (x, snd (h x))

ones = 1 : ones

what = what

-- pairs = [ (x, y) | x <- [0..], y <- [0..] ]
-- pairs = [ (x, y) | x <- [0..], y <- [0..], z <- [0..], x + y == z ]
pairs = [ (x, z - x) | z <- [0..], x <- [0..z] ]

-- >>> take 10 pairs
-- [(0,0),(0,1),(1,0),(0,2),(1,1),(2,0),(0,3),(1,2),(2,1),(3,0)]

pythagoreanTriples = [ (x, y, z) | z <- [0..], y <- [0..z], x <- [0..y],
                            x^2 + y^2 == z^2, gcd x y == 1 ]

noSpaces :: IO ()
noSpaces = do text <- getContents
              putStr (filter (/= ' ') text)

v x = (+x)

u = (^2)

-- >>> (u . v 3) 5
-- 64

-- Зад.1
complAdd p1 p2 = (fst p1 + fst p2, snd p1 + snd p2)
-- pattern matching за "декомпозиране" на съставни части - очевидно по-удобно
complSub (a,b) (c,d) = (a - c, b - d)
complMul (a,b) (c,d) = (a*c - b*d, a*d + b*c)
-- Еквивалентът без pattern matching - изключително податлив на грешки
--complMul p1 p2 = (fst p1 * fst p2 - snd p1 * snd p2, fst p1 * snd p2 + snd p1 * fst p2)

-- Зад.2
distance (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2

-- Interlude: примери за удобството от pattern matching за списъци
--length' lst
--  | null lst  = 0
--  | otherwise = 1 + length' (tail lst)

--length' [] = 0
--length' (x:xs) = 1 + length' xs

--take' n lst
--  | null lst  = []
--  | n == 0    = []
--  | otherwise = (head lst) : take' (n-1) (tail lst)

take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x : take' (n-1) xs

-- Примери за разликата м/у ляво и дясно свиване
f lst = foldr (\el res -> el:res) [] lst
g lst = foldl (\res el -> el:res) [] lst

-- Зад.3
minimum' lst = foldl min (head lst) (tail lst)
maximum' lst = foldl max (head lst) (tail lst)
reverse' lst = foldl (flip (:)) [] lst
length' lst = foldl (\res _ -> res + 1) 0 lst
-- Същата забележка както в Scheme - няма short circuit
all' p lst = foldl (\res el -> res && p el) True lst
any' p lst = foldl (\res el -> res || p el) False lst
replicate' n x = foldl (\res _ -> x : res) [] [1..n]

-- List comprehension надгражда както map, така и filter
map' f lst = [ f x | x<-lst ]
filter' p lst = [ x | x<-lst, p x ]

-- Зад.4
divisorsCount n = length [ k | k<-[1..n], mod n k == 0 ]

prime 1 = False
prime n = null [ k | k<-[2..sqn], mod n k == 0 ]
  where sqn = floor $ sqrt $ fromIntegral n -- по-хубава горна граница

descartes lst1 lst2 = [ (x,y) | x<-lst1, y<-lst2 ]

-- Зад.5
primes = filter prime [2..]

-- Зад.6
primes' = sieve [2..]
  where sieve (x:xs) = x : sieve [ k | k<-xs, mod k x /= 0 ]

-- Зад.1 - тривиалното решение е еквивалентно на два вложени безкрайни цикъла,
-- при което никога няма да се достигне до втората итерация на външния.
-- Тук всяка двойка ще се генерира за крайно време.
natPairs = [ (x, diag - x + 1) | diag<-[1..], x<-[1..diag]]

-- Зад.2
-- Основна помощна функция (може и да я разпишем на ръка)
countMyHead :: Eq a => [a] -> Int
countMyHead lst = length $ takeWhile (== head lst) lst

compress :: Eq a => [a] -> [(a,Int)]
compress [] = []
compress lst = (head lst, count) : compress (drop count lst)
  where count = countMyHead lst

-- Зад.3
maxRepeated :: Eq a => [a] -> Int
--maxRepeated lst = maximum (map snd (compress lst))
--maxRepeated lst = maximum $ map snd $ compress $ lst
--maxRepeated lst = maximum . map snd . compress $ lst
maxRepeated = maximum . map snd . compress

-- Зад.4
-- Може да използваме всички подходи, които бяхме разгледали на Scheme
makeSet :: Eq a => [a] -> [a]
makeSet [] = []
makeSet (x:xs)
  | x `elem` xs = makeSet xs
  | otherwise   = x : makeSet xs

-- Зад.5
histogram :: Eq a => [a] -> [(a, Int)]
--histogram lst = map (\x -> (x,count x lst)) (makeSet lst)
histogram lst = [ (x, count x lst) | x<-makeSet lst ]
  where count x lst = length $ filter (==x) lst

-- Зад.6
maxDistance :: [(Float,Float)] -> Float
maxDistance pts = maximum [ dist p1 p2 | p1<-pts, p2<-pts ]
  where dist (a,b) (c,d) = sqrt $ (a-c)^2 + (b-d)^2

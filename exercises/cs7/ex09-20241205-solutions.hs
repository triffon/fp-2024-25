isLengthMoreThan1 :: [a] -> Bool
--isLengthMoreThan1 lst = not (null lst) && not (null (tail lst))
isLengthMoreThan1 [] = False
--isLengthMoreThan1 (x:[]) = False  -- еквивалентно на горното
isLengthMoreThan1 [x] = False
isLengthMoreThan1 _ = True

-- Стандартно сортиране в ненамаляващ ред
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort [x] = [x] -- може и без тази клауза, тя е оптимизация
quickSort (x:xs) = quickSort [ y | y<-xs, y < x ]
                ++ [x]
                ++ quickSort [ y | y<-xs, y >= x ] -- Важно: само y > x ще изпуска повтарящи се елементи

-- Сортиране по критерий тип "трябва ли x да е преди y в наредбата"
quickSortBy :: (a -> a -> Bool) -> [a] -> [a]
quickSortBy _ [] = []
quickSortBy _ [x] = [x] -- може и без тази клауза, тя е оптимизация
quickSortBy cmp (x:xs) = quickSortBy cmp [ y | y<-xs, cmp y x ]
                ++ [x]
                ++ quickSortBy cmp [ y | y<-xs, not (cmp y x) ] -- Аналогично: cmp x y ще изпуска повтарящи се елементи

-- Примерно използване на по-сложен предикат
-- Проблем: той се извиква при всяко сравнение на два елемента
--maxLength lsts = quickSortBy (\x y -> length x < length y) lsts

-- Two-step sorting: използва допълнителна памет за кеширане на резултатите от функцията, ако са скъпи за преизчисление
-- Очевидно ненужно в случаите, в които функцията е "бърза".
quickSortOn :: Ord b => (a -> b) -> [a] -> [a]
quickSortOn f lst = map fst $ quickSortBy (\p1 p2 -> snd p1 < snd p2) [ (x, f x) | x<-lst ]

-- От миналия път
makeSet :: Eq a => [a] -> [a]
makeSet [] = []
makeSet (x:xs)
  | x `elem` xs = makeSet xs
  | otherwise   = x : makeSet xs

histogram :: Eq a => [a] -> [(a, Int)]
histogram lst = [ (x, length $ filter (==x) lst) | x<-makeSet lst ]

mostFrequent :: Ord a => [a] -> a
-- 90% вярно: нямаме контрол над избора м/у еднакво чести ел-ти => трябва вторичен критерий
--mostFrequent lst = last $ quickSortOn (\x -> count x lst) lst
-- Неефикасно: няма нужда да сортираме целия списък, трябва ни аналогично maximumBy/maximumOn
mostFrequent = fst . head . quickSortBy cmp' . histogram
  where cmp' (x,c1) (y,c2) = c1 > c2 || (c1 == c2 && x > y)
        --cmp' (x,c1) (y,c2)
        --  | c1 > c2   = True
        --  | c1 < c2   = False
        --  | otherwise = x > y

specialSort :: Ord a => [[a]] -> [[a]]
specialSort lst = quickSortOn mostFrequent lst

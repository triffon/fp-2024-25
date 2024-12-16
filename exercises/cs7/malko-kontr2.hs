-- Общи, полезни функции
count :: Eq a => a -> [a] -> Int
count x xs = length $ filter (==x) xs

-- Можем и без нея, макар условието да подсказва че трябва (даже два пъти!)
uniques :: Eq a => [a] -> [a]
uniques = foldr (\el res -> if el `elem` res then res else el:res) []

-- Зад.1
isInteresting :: [Int] -> Bool
isInteresting xs = all ((<=2) . (`count` xs)) xs

-- Зад.2
dups :: [Int] -> [Int]
dups xs = uniques [ x | x<-xs, count x xs == 2 ]

-- Наивният подход тук всъщност е по-хубав (използва, че списъкът е интересен)
dups' :: [Int] -> [Int]
dups' [] = []
dups' (x:xs)
  | x `elem` xs = dups' xs
  | otherwise   = x : dups' xs

-- Зад.3
intersect :: [Int] -> [Int] -> [Int]
intersect xs ys = [ x | x<-xs, x `elem` ys ]

allDups :: [[Int]] -> [Int]
allDups xss = foldr1 intersect $ map dups xss

-- При търсене на общите елементи няма нужда да сравняваме всяко със всяко,
-- можем само да проверим кои от първия списък присъстват в другите
allDups' :: [[Int]] -> [Int]
allDups' [] = []
allDups' xss = [ x | x<-ds, all (x `elem`) rest ]
  where (ds:rest) = map dups xss

-- Зад.4
-- Обикновено в задачи с индекси има и решения със zip lst [0..]
findIndex :: Eq a => a -> [a] -> Int
findIndex x xs = loop xs 0
  where loop [] _ = error "Should've been there"
        loop (y:ys) i
          | x == y    = i
          | otherwise = loop ys (i+1)

findIndex' :: Eq a => a -> [a] -> Int
findIndex' x xs = length $ takeWhile (/=x) xs

lastIndex :: Eq a => a -> [a] -> Int
lastIndex x xs = length xs - 1 - findIndex' x (reverse xs)

furthestDup :: [Int] -> Int
furthestDup [] = 0 -- Разстояние от 0 е невалидно
furthestDup (x:xs)
  | x `elem` xs = max (succ $ findIndex' x xs) (furthestDup xs) -- Срещнали сме x веднъж, да видим колко места има до следващото
  | otherwise   = furthestDup xs

furthestDup' :: [Int] -> Int
furthestDup' xs = maximum [ lastIndex x xs - findIndex' x xs | x<-dups xs ]

furthestDup2 :: [Int] -> Int
furthestDup2 xs = maximum . map findDist . dups $ xs
  where ds = dups xs
        pairs = filter (\(x,_) -> x `elem` ds) $ zip xs [0..]
        findDist x = let [i,j] = [ i | (val,i)<-pairs, val == x ] in j-i

-- Бонус: числата, които биха променили резултата от allDups при прибавяне веднъж
-- към всеки списък, са тези които вече се срещат точно по веднъж във всеки списък.
-- Ако такива същвствуват, отговорът е най-малкото измежду тях и задачата е не по-сложна от зад.3.
bonus :: [[Int]] -> Maybe Int
bonus xss = case foldr1 intersect $ map loners xss of [] -> Nothing
                                                      ls -> Just $ maximum ls
  where loners xs = filter ((==1).(`count` xs)) xs

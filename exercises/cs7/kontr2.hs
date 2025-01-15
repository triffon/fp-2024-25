-- Зад.1
-- Всички точки от първи квадрант, вкл. x=0 и изкл. y=0
firstQuad :: [(Int, Int)]
firstQuad = [ (x, diag - x) | diag<-[1..], x<-[0..diag-1]]

-- Списък от четирите ротации на дадена точка (без симетриите!)
rotations :: (Int,Int) -> [(Int,Int)]
rotations (x,y) = [(x,y),(-y,x),(-x,-y),(y,-x) ]

outsideCircle :: Int -> Int -> Int -> [(Int,Int)]
outsideCircle cx cy r = filter out plane
  where plane = (0,0) : concatMap rotations firstQuad
        out (x,y) = (x-cx)*(x-cx) + (y-cy)*(y-cy) > r*r

-- Зад.2
data DeepList a = Atom a | List [DeepList a]
instance Show a => Show (DeepList a) where
  show (Atom x) = show x
  show (List xs) = show xs

deepMapCond :: DeepList a -> (a -> Int -> Bool) -> (a -> Int -> b) -> (a -> Int -> b) -> DeepList b
deepMapCond lst p f1 f2 = helper 0 lst
  where helper d (Atom x) = Atom $ (if p x d then f1 else f2) x d
        helper d (List xs) = List $ map (helper (d+1)) xs

dl, dl2 :: DeepList Int
dl = List [Atom 1, List [Atom 2, List [Atom 5, Atom 1], Atom 4], Atom 3]
dl2 = deepMapCond dl (>) (\x d -> d) (\x d -> x*2)

-- Зад.3
-- Приема двуместен предикат, който казва дали първият аргумент трябва да е преди втория в сортирания списък
quickSort :: (a -> a -> Bool) -> [a] -> [a]
quickSort _ [] = []
quickSort _ [x] = [x]
quickSort p (x:xs) = quickSort p [ y | y<-xs, p y x ] ++ [x] ++ quickSort p [ y | y<-xs, not $ p y x ]

uniques :: Eq a => [a] -> [a]
uniques [] = []
uniques (x:xs)
  | x `elem` xs = uniques xs
  | otherwise   = x : uniques xs

type Driver = (String,Int,Int,Int)
type Race = (String, [Driver])
type Season = [Race]

getName :: Driver -> String
getName (name,_,_,_) = name

-- Няма нужда да сортираме целия списък, но ни трябва аналогична minimum/maximum с предикат
-- Заб.: повторението може да се избегне с функция от по-висок ред, аналогична на Data.Function.on
winner, fastest :: Race -> String
winner (_,xs) = getName . head $ quickSort (\(_,pts1,_,_) (_,pts2,_,_) -> pts1 > pts2) xs
fastest (_,xs) = getName . head $ quickSort (\(_,_,_,t1) (_,_,_,t2) -> t1 < t2) xs

-- a)
allWinners :: Season -> [String]
allWinners = uniques . map winner

-- б)
penaltyImpact :: Season -> [(String,Int)]
penaltyImpact s = quickSort (\(_,t1) (_,t2) -> t1 > t2) totals
  where -- Всички двойки (пилот,наказание) през сезона
        allPens = [ (name,p) | (name,_,p,_)<-concatMap snd s, p > 0 ]
        -- Уникалните имена на наказаните пилоти
        penalized = uniques $ map fst allPens
        -- За всеки наказан пилот, сумата от неговите наказания
        totals = [ (name, sum [ t | (n,t)<-allPens, name == n ]) | name<-penalized ]

-- в)
-- Събираме данните от всички състезания в едно (с неудобството, че нашата fastest приема наредена двойка)
fastestLap :: Season -> String
fastestLap s = fastest $ ("", concatMap snd s)

-- г)
missedFastestLapWins :: Season -> [String]
missedFastestLapWins s = [ fst r | r<-s, winner r /= fastest r ]

season :: Season
season = [("Silverstone", [("Leclerc", 15, 10, 82), ("Hamilton", 25, 5, 80),
                           ("Verstappen", 18, 0, 78)]),
          ("Monza", [("Sainz", 18, 0, 81), ("Hamilton", 15, 10, 79),
                     ("Verstappen", 25, 0, 77)]),
          ("Spa", [("Norris", 18, 5, 78), ("Russell", 25, 0, 76)]) ]

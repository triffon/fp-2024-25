-- Скелет на зад.1б) от примерната тема
-- Връща цикъла, съдържащ first (и започващ с него)
makeCycle :: Eq a => (a -> a) -> a -> [a]
makeCycle f first = first : makeCycleHelper f first (f first)
  where makeCycleHelper f first curr
         | curr == first = []
         | otherwise     = curr : (makeCycleHelper f first (f curr))

-- Връща списък от всички цикли на дадена n-пермутация.
-- Останалата част от задачата е maximumBy.
allCycles :: (Num a, Enum a, Eq a) => a -> (a -> a) -> [[a]]
allCycles n f = allCyclesHelper [0..n-1]
  where -- Връща всички цикли, включващи числата от дадения списък
        allCyclesHelper [] = []
        allCyclesHelper xs = c : allCyclesHelper [ x | x<-xs, not (x `elem` c) ]
          where c = makeCycle f (head xs)

-- Основната част на зад.3 от миналогодишното второ контролно -
-- генериране на поток от всички правилни дроби.
-- За разлика от потока от наредени двойки, тук и наивното генериране е "продуктивно"
pairs = map makePair [ (x,y) | y<-[2..], x<-[1..y-1]]
  where makePair (x,y)
          | even x    = ((x,y),(y,x `div` 2))
          | otherwise = ((x,y),(y*2,x))

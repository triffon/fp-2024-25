main :: IO()
main = do
    print $ isNull [] == True
    print $ isNull [1 .. 5] == False
    print $ isNull ['a', 'b'] == False
    print $ isNull [1.75 .. ] == False

    print $ firstEl [1] == 1
    print $ firstEl [10, 5, 6] == 10
    print $ firstEl ["I am", "You are"] == "I am"

    print $ removeFirst [5, 2, 3] == [2, 3]
    print $ removeFirst ["OOpps"] == [] 

    print $ fromXs 5 [1, 2, 3, 4, 5, 6] == [6]
    print $ fromXs 5 ["A", "B", "C"] == []

    print $ concatenate [1, 2, 3] [5, 8, 9] == [1, 2, 3, 5, 8, 9]
    print $ concatenate [1] [5, 8, 9] == [1, 5, 8, 9]
    print $ concatenate ['1', '2', '3'] ['5'] == "1235"

    print $ join [[1, 2], [5, 6], [9]] == [1, 2, 5, 6, 9]
    print $ join [['H'], ['a', 's'], "kell"] == "Haskell"

    print $ joinFold [[1, 2], [5, 6], [9]] == [1, 2, 5, 6, 9]
    print $ joinFold [['H'], ['a', 's'], "kell"] == "Haskell"

isNull :: [a] -> Bool
isNull [] = True
isNull _ = False

firstEl :: [a] -> a
firstEl [] = error "Empty list"
firstEl (x:_) = x

removeFirst :: [a] -> [a]
removeFirst [] = []
removeFirst (_:xs) = xs

fromXs :: Int -> [a] -> [a]
fromXs 0 xs = xs
fromXs _ [] = []
fromXs index (_:xs) = fromXs (index - 1) xs

concatenate :: [a] -> [a] -> [a]
concatenate [] ys = ys
concatenate (x:xs) ys = x : concatenate xs ys

join :: [[a]] -> [a]
join [] = []
join (xs:xss) = xs ++ join xss

joinFold :: [[a]] -> [a]
joinFold = foldl concatenate []
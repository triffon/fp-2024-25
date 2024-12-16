import Data.List

main :: IO()
main = do
    print $ sumUnique [[1,2,3,2],[-4,-4],[5]] == 9 -- (= 1 + 3 + 5)
    print $ sumUnique [[2,2,2],[3,3,3],[4,4,4]] == 0
    print $ sumUnique [[1,2,3],[4,5,6],[7,8,9]] == 45

sumUnique :: [[Int]] -> Int
sumUnique xss = sum $ concat $ filter (\ xs -> length xs == 1) $ group $ sort $ concat xss
-- sumUnique = sum . concat . filter (\ xs -> length xs == 1) . group . sort . concat
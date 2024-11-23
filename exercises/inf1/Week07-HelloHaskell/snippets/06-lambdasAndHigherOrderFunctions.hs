getEven :: [Int] -> [Int]
getEven xs = filter (\ x -> even x) xs

double :: [Int] -> [Int]
double xs = map (\ x -> 2 * x) xs

areAllOdd :: [Int] -> Bool -- any
areAllOdd xs = all odd xs

main :: IO ()
main = do
    print $ (\ x y z -> x + y + z) 1 2 3
main :: IO()
main = do
    print $ gen_KS 3 10 == [[0,0,10], [0,1,9], [0,2,8], [0,3,7], [0,4,6], [0,5,5], [0,6,4], [0,7,3], [0,8,2], [0,9,1], [0,10,0], [1,0,9], [1,1,8], [1,2,7], [1,3,6], [1,4,5], [1,5,4], [1,6,3], [1,7,2], [1,8,1], [1,9,0], [2,0,8], [2,1,7], [2,2,6], [2,3,5], [2,4,4], [2,5,3], [2,6,2], [2,7,1], [2,8,0], [3,0,7], [3,1,6], [3,2,5], [3,3,4], [3,4,3], [3,5,2], [3,6,1], [3,7,0], [4,0,6], [4,1,5], [4,2,4], [4,3,3], [4,4,2], [4,5,1], [4,6,0], [5,0,5], [5,1,4], [5,2,3], [5,3,2], [5,4,1], [5,5,0], [6,0,4], [6,1,3], [6,2,2], [6,3,1], [6,4,0], [7,0,3], [7,1,2], [7,2,1], [7,3,0], [8,0,2], [8,1,1], [8,2,0], [9,0,1], [9,1,0], [10,0,0]]

nats :: [Int]
nats = [1 ..]

gen_KS :: Int -> Int -> [[Int]]
gen_KS 1 s = [[s]]
gen_KS k s = [ h : t | h <- [0 .. s], t <- gen_KS (k - 1) (s - h)]
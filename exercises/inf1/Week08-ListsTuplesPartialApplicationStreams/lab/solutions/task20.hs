main :: IO()
main = do
    print $ (take 10 $ gen_K_tuples 3) == [[0,0,1], [0,1,0], [1,0,0], [0,0,2], [0,1,1], [0,2,0], [1,0,1], [1,1,0], [2,0,0], [0,0,3]]

nats :: [Int]
nats = [1 ..]

gen_KS :: Int -> Int -> [[Int]]
gen_KS 1 s = [[s]]
gen_KS k s = [ h : t | h <- [0 .. s], t <- gen_KS (k - 1) (s - h)]

gen_K_tuples :: Int -> [[Int]]
gen_K_tuples k = [ x | s <- nats, x <- gen_KS k s]

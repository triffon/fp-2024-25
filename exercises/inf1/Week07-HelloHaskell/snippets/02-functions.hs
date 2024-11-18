main :: IO()
main = do
    print $ "Hello world!"
    print "Hello world!"
    print $ 2
    print $ 2 + 3

f :: Int -> Int
f n = n + 1

g :: Int -> Int -> Int
g a b = 2 * a + b
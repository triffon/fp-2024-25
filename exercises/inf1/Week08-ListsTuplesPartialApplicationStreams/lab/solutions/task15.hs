main :: IO()
main = do
    print $ (take 8 $ myIterate (*2) 5) == [5, 10, 20, 40, 80, 160, 320, 640]

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)
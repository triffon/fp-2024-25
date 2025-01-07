main :: IO ()
main = do
    a :: Int <- readLn
    b :: Int <- readLn
    print $ a + b

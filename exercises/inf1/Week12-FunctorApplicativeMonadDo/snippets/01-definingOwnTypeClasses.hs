main :: IO()
main = do
    print $ toString (123 :: Int)
    print $ add (5 :: Int) (7 :: Int)
    print $ printAndAdd (5 :: Int) (7 :: Int)

class Printable a where
    toString :: a -> String

instance Printable Int where
    toString x = "Integer: " ++ show x

class Addable a where
    add :: a -> a -> a

instance Addable Int where
    add x y = x + y

class (Printable a, Addable a) => PrintableAndAddable a where
    printAndAdd :: a -> a -> String

instance PrintableAndAddable Int where
    printAndAdd x y = "Sum of " ++ toString x ++ " and " ++ toString y ++ " is: " ++ toString (add x y)
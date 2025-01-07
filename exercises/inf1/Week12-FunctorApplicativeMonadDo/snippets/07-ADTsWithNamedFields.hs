main :: IO()
main = do
    print $ Person {name = "Peter Kolev", age = 22, height = 180.5}

data Person = Person
  { name :: String, 
    age :: Int,
    height :: Double
  } deriving (Show)
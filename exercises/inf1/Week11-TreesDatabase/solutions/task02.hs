main :: IO()
main = do
    print $ db

    print $ getTotal db
    print $ getTotal db == 31.2

    -- print $ "Buying 500 Bread:"
    -- print $ buy "Bread" 500 db
    -- print $ "Buying 500 Water:"
    -- print $ buy "Water" 500 $ buy "Bread" 500 db
    -- print $ "Buying 100 Soap:"
    -- print $ buy "Soap" 100 $ buy "Water" 500 $ buy "Bread" 500 db
    -- print $ "Buying 500 Bread:"
    -- print $ buy "Bread" 500 $ buy "Soap" 100 $ buy "Water" 500 $ buy "Bread" 500 db
    -- print $ buy "Water" 100 $ buy "Bread" 500 $ buy "Soap" 100 $ buy "Water" 500 $ buy "Bread" 500 db -- Should give an error.
    -- print $ buy "Soap" 200 $ buy "Bread" 500 $ buy "Soap" 100 $ buy "Water" 500 $ buy "Bread" 500 db -- Should give an error.

type Name = String
type Quantity = Int
type Price = Double
type Database = [Product]

data Product = Product Name Quantity Price
    deriving (Show)

data ShopError = ProductNotFoundError | NotEnoughQuantityError

db :: Database
db = [Product "Bread" 1000 1.20, 
      Product "Milk" 2000 4.50,
      Product "Lamb" 5000 10,
      Product "Cheese" 750 5,
      Product "Butter" 5 5.50,
      Product "Water" 500 0.50,
      Product "Soap" 250 4.50]

getPrice :: Product -> Price
getPrice (Product _ _ price) = price

getTotal :: Database -> Price
getTotal = sum . map getPrice

buy :: Name -> Quantity -> Database -> Either ShopError Database
buy _ _ [] = Left ProductNotFoundError
buy name quantity (p@(Product pName pQuantity pPrice):ps)
 | name == pName && quantity == pQuantity = Right ps
 | name == pName && quantity < pQuantity = Right (Product pName (pQuantity - quantity) pPrice) : ps
 | name == pName && quantity > pQuantity = Left NotEnoughQuantityError
 | otherwise = buy name quantity ps

db1 :: Database
db1 = [ Product "Bread" 1000 1.20, Product "Milk" 2000 4.50, Product "Lamb" 5000 10.00, Product "Cheese" 750 5.00, Product "Butter" 1000 5.50, Product "Water" 500 0.50, Product "Soap" 250 4.50 ]

db2 :: Database
db2 = [ Product "Bread" 1000 1.20, Product "Milk" 2000 4.50, Product "Lamb" 5000 10.00, Product "Cheese" 750 5.00, Product "Lamb" 1000 5.50, Product "Water" 500 0.50, Product "Lamb" 250 4.50 ]
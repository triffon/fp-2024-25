import Data.Char
import Data.List

main :: IO()
main = do
    print $ ascendingleftSuffix 37563 == 36
    print $ ascendingleftSuffix 32763 == 367
    print $ ascendingleftSuffix 32567 == 7
    print $ ascendingleftSuffix 32666 == 6

isSorted :: [Char] -> Bool
ascendingleftSuffix :: Int -> Int

isSorted xs = xs == (sort xs)

ascendingleftSuffix n = read $ last $ filter isSorted $ inits $ nub $ reverse $ show n
-- ascendingleftSuffix = read . last . filter isSorted . inits . nub . reverse . show
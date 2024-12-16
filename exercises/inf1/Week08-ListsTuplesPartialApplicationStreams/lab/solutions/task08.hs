import Data.Char

main :: IO()
main = do
    print $ sumSpecialPrimes 5 2 == 392
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462

intSqrt :: Int -> Int
isPrime :: Int -> Bool
constains :: Int -> Int -> Bool
sumSpecialPrimes :: Int -> Int -> Int

intSqrt = floor . sqrt . fromIntegral

isPrime n = n /= 1 && null [ x | x <- [2 .. intSqrt n], mod n x == 0]

constains digit number = elem digit $ map digitToInt $ show number

sumSpecialPrimes n d = sum $ take n [x | x <- [2 ..], isPrime x, constains d x]
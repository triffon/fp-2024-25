module Main where

parseList :: IO [Int]
parseList = map read . words <$> getLine

sublists :: [t] -> [[t]]
sublists [] = [[]]
sublists (x:xs) = map (x:) (sublists xs) ++ sublists xs

sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve (filter ((/= 0) . (`rem` x)) xs)

primes :: [Int]
primes = sieve [2..]

isPrime :: Int -> Bool
isPrime n = n `elem` takeWhile (<= n) primes

main :: IO ()
main = parseList >>= mapM_ print . sublists . filter isPrime
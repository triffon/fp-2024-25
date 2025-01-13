module Main where

parseList :: IO [Int]
parseList = map read . words <$> getLine

sort :: Ord t => [t] -> [t]
sort [] = []
sort (x:xs) = sort (filter (< x) xs) ++ [x] ++ sort (filter (>= x) xs) 

main :: IO ()
main = parseList >>= putStrLn . unwords . map show . sort
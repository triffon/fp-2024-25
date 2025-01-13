module Main where

-- main :: IO ()
-- main = do
--   line <- getLine
--   let reversedSentence = reverse $ words line
--   print $ unwords reversedSentence

main :: IO ()
main = getLine >>= print . unwords . reverse . words
module Main where

main :: IO ()
main = do
  filename <- getLine
  word <- getLine
  contents <- readFile filename
  print $ length $ filter (== word) $ words contents
  
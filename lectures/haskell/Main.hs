module Main where

import Control.Monad

-- >>> :k IO
-- IO :: * -> *

main :: IO ()
-- main = putStrLn "Hello, Haskell!"
-- !!! main = putStrLn ("Въведохте: " ++ getLine)

{-
main = do line <- getLine
          putStrLn ("Въведохте: " ++ line)
-}

main = do putStrLn "Моля, въведете палиндром: "
          line <- getLine
--          let revLine = reverse line
--          if revLine == line then putStrLn "Благодаря!"
          if reverse line == line then putStrLn "Благодаря!"
          else do putStrLn $ line ++ " не е палиндром!"
                  main

getInt :: IO Int
getInt = do line <- getLine
            return (read line)

test = do x <- putStrLn "Hello"
          return x

findAverage :: IO Double
findAverage = do putStr "Моля, въведете брой: "
                 n <- getInt
                 s <- readAndSum n
                 return (fromIntegral s / fromIntegral n)

readAndSum :: Int -> IO Int
readAndSum 0 = return 0
readAndSum n = do putStr "Моля, въведете число: "
                  x <- getInt
                  s <- readAndSum (n - 1)
                  return (x + s)

getInts :: Int -> IO [Int]
getInts n = sequence $ replicate n getInt

printRead s = do putStr $ s ++ " = "
                 getInt

readInt :: String -> IO Int
readInt s = do putStr $ "Моля, въведете " ++ s ++ ": "
               getInt

findAverage2 :: IO Double
findAverage2 = do n <- readInt "брой"
                  l <- mapM (readInt.("число #"++).show) [1..n]
                  let s = sum l
                  return $ fromIntegral s / fromIntegral n

main2 = forever $
        do avg <- findAverage2
           putStrLn $ "Средното аритметично е: " ++ show avg
           putStrLn "Хайде отново!"

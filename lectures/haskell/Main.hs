module Main where

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

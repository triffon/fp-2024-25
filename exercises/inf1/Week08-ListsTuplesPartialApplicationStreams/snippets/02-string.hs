import Data.Char

numbertoString :: Int -> String
numbertoString n = show n

stringToNumber :: String -> Int
stringToNumber str = read str

charToDigit :: Char -> Int
charToDigit ch = digitToInt ch
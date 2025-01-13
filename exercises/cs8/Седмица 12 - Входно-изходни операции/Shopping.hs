module Main where
import System.IO (readFile')
import System.Random (randomR)

data Command = 
  Add String Int |
  Remove String |
  List |
  Exit
  deriving Show  

type ShoppingList = [(String, Int)]

split :: String -> Char -> [String]
split str delimeter = splitHelper str delimeter ""
  where splitHelper :: String -> Char -> String -> [String]
        splitHelper "" _ word = [word]
        splitHelper (x:xs) delimeter word
          | x == delimeter = word : splitHelper xs delimeter ""
          | otherwise = splitHelper xs delimeter $ word ++ [x]

parseLine :: String -> (String, Int)
parseLine str = let args = words str
  in (head args, read (last args))

parseShoppingList :: String -> ShoppingList
parseShoppingList str = map parseLine $ split str '\n'

parseCommand :: String -> Command
parseCommand "exit" = Exit
parseCommand "list" = List
-- TODO:

execCommand :: Command -> ShoppingList
execCommand = undefined

main :: IO ()
main = do
  print "Enter file name: "
  filename <- getLine
  contents <- parseShoppingList <$> readFile' filename
  print contents
  print "Enter a command: "
  command <- parseCommand <$> getLine
  print command
main :: IO()
main = do
    print $ isPalindrome 6 == True
    print $ isPalindrome 1010 == False
    print $ isPalindrome 505 == True
    print $ isPalindrome 123321 == True
    print $ isPalindrome 654 == False

getReversedNumber :: Int -> Int
isPalindrome :: Int -> Bool

getReversedNumber n = helper n 0
 where 
    helper :: Int -> Int -> Int
    helper 0 result = result
    helper current result = helper (div current 10) (result * 10 + mod current 10)

isPalindrome n = n == getReversedNumber n
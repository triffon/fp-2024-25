main :: IO()
main = do
    print $ sumMinMaxPalindromes 132465 == 8
    print $ sumMinMaxPalindromes 654546 == 8
    print $ sumMinMaxPalindromes 100001 == 100012
    print $ sumMinMaxPalindromes 21612 == 21614
    print $ sumMinMaxPalindromes 26362 == 26364

revOneLineMagic :: Int -> Int
revOneLineMagic = read . reverse . show

isPalindrome :: Int -> Bool
isPalindrome n = n == revOneLineMagic n

sumMinMaxPalindromes :: Int -> Int
sumMinMaxPalindromes n = maximum helper + minimum helper
 where
    helper = [x | x <- [2 .. n], mod n x == 0 && isPalindrome x]
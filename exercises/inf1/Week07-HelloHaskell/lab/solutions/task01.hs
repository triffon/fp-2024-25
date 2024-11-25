main :: IO()
main = do
    print $ minIf 15 60 == 15
    print $ minIf 60 15 == 15
    print $ minGuard 15 60 == 15
    print $ minGuard 60 15 == 15
    print $ minBuiltIn 60 15 == 15

    print $ lastDigit 154 == 4

    print $ quotientWhole 64 2 == 32
    print $ divWhole 154 17 == 9.058823529411764

    print $ removeLastDigit 154 == 15    

    print $ divReal 154.451 10.01 == 15.42967032967033
    print $ quotientReal 154.21 17.17 == 8

    print $ avgWhole 5 1542 == 773.5

    print $ roundTwoDig 3.1413465345321 == 3.14
    print $ roundTwoDigButWithMagic 3.1413465345321 == 3.14

minIf :: Int -> Int -> Int
minGuard :: Int -> Int -> Int
minBuiltIn :: Int -> Int -> Int
lastDigit :: Int -> Int
quotientWhole :: Int -> Int -> Int
divWhole :: Int -> Int -> Double
removeLastDigit :: Int -> Int
divReal :: Double -> Double -> Double
quotientReal :: Double -> Double -> Int
avgWhole :: Int -> Int -> Double
roundTwoDig :: Double -> Double
roundTwoDigButWithMagic :: Double -> Double

minIf a b = if a < b then a else b

minGuard a b
 | a < b = a
 | otherwise = b

minBuiltIn a b = min a b

lastDigit n = mod n 10

quotientWhole a b = div a b

divWhole a b = fromIntegral a / fromIntegral b

removeLastDigit n = div n 10

divReal a b = a / b

quotientReal a b = truncate $ a / b

avgWhole a b = fromIntegral (a + b) / 2

roundTwoDig n = (fromIntegral $ truncate $ n * 100) / 100

roundTwoDigButWithMagic = (/ 100) . fromIntegral . truncate . (* 100)
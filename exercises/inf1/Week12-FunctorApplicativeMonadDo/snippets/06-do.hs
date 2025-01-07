main :: IO()
main = do
    print $ complexOperation 10 2
    print $ complexOperation 10 0
    print $ complexOperation 10 (-2)

safeDivide :: Double -> Double -> Either String Double
safeDivide _ 0 = Left "division by 0"
safeDivide x y = Right (x / y)

safeRoot :: Double -> Either String Double
safeRoot x
    | x < 0 = Left "square root of a negative number"
    | otherwise = Right (sqrt x)

complexOperation :: Double -> Double -> Either String Double
complexOperation a b = do
    res1 <- safeDivide a b
    res2 <- safeRoot res1
    return res2
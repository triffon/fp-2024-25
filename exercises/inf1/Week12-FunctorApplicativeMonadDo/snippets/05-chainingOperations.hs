main :: IO()
main = do
    let result1 = safeDivide 10 2 >>= safeRoot
    print $ result1

    let result2 = safeDivide 10 0 >>= safeRoot
    print result2

safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

safeRoot :: Double -> Maybe Double
safeRoot x
    | x < 0 = Nothing
    | otherwise = Just (sqrt x)

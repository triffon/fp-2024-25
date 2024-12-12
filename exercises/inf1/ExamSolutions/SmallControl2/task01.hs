triangleTriplets :: Int -> [(Int, Int, Int)]
triangleTriplets s = [(a, b, c) | c <- [1 ..], a <- [1 .. c], b <- [1 .. c], getAreaRightTriangle a b >= s && isRightTriangle a b c]
 where
    isRightTriangle :: Int -> Int -> Int -> Bool
    isRightTriangle a b c = a^2 + b^2 == c^2
    getAreaRightTriangle :: Int -> Int -> Int
    getAreaRightTriangle a b = floor ((fromIntegral (a * b)) / 2.0)

main :: IO()
main = do
    print $ (take 10 $ triangleTriplets 30) == [(5,12,13),(12,5,13),(9,12,15),(12,9,15),(8,15,17),(15,8,17),(12,16,20),(16,12,20),(7,24,25),(24,7,25)]
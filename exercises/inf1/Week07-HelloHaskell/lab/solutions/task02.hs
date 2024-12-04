main :: IO()
main = do
    print $ factRec 11 == 39916800
    print $ factPM 11 == 39916800
    print $ factIter 11 == 39916800
    print $ factXs 11 == 39916800

factRec :: Int -> Int
factPM :: Int -> Int
factIter :: Int -> Int
factXs :: Int -> Int

factRec n = if n < 1 then 1 else n * factRec (n - 1)

factPM 0 = 1
factPM n = n * factPM (n - 1)

factIter n = helper n 1
 where
    helper :: Int -> Int -> Int
    helper 0 result = result
    helper current result = helper (current - 1) (result * current)

factXs n = product [1 .. n]
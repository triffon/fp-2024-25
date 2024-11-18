import Prelude hiding (repeat)

f :: Int -> Int -> Int
f a b = a + b

factorial :: Int -> Int
factorial n = if n == 0 then 1 else n * factorial (n - 1)

factorial' :: Int -> Int
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

factorial'' :: Int -> Int
factorial'' n
  | n == 0 = 1
  | n > 0 = n * factorial'' (n - 1)
  | otherwise = error "Negative number was provided"

factorial''' :: Int -> Int
factorial''' n = case n of
  0 -> 1
  p -> p * factorial''' (p - 1)

roots :: Double -> Double -> Double -> Int
roots a b c
  | let a = 5, discriminant > 0 = 2
  | discriminant == 0 = 1
  | otherwise = 0
  where discriminant :: Double
        discriminant = b ** 2 - 4 * a * c

repeat :: (a -> a) -> Int -> a -> a
repeat _ 0 = id
repeat f n = \x -> f (repeat f (n - 1) x)

compute :: (Int, Int, Double) -> Double
compute (0, _, _) = 0
compute (a, b, c) = let v = c / fromIntegral a  
                        u = 1 + 1
  in v + fromIntegral b
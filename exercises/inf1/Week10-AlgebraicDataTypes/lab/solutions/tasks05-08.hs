import Data.Maybe

main :: IO()
main = do
    print $ getPoints (\x -> x * x) (2+) [Point2D 2 2, Point2D 1 2, Point2D 3 7] == [Point2D 2 2, Point2D 3 7]

    print $ distance (Point2D 2 5) (Point2D 6 9) == Just 5.66
    print $ distance (Point3D 2 5 10) (Point3D 6 9 (-5)) == Just 16.03
    print $ distance (Point2D 2 5) (Point3D 6 9 (-5)) == Nothing

    print $ unsafeDistance (Point2D 2 5) (Point2D 6 9) == 5.66
    print $ unsafeDistance (Point3D 2 5 10) (Point3D 6 9 (-5)) == 16.03

    print $ closestTo (Point3D 2 5 10) [(Point3D 4 5 6), (Point3D 5 2 (-10)), (Point3D (-2) 1 45), (Point3D 12 0 2), (Point3D 6 5 4)] == Just [Point3D 4.0 5.0 6.0]
    print $ closestTo (Point2D 2 5) [(Point3D 4 5 6), (Point3D 5 2 (-10)), (Point3D (-2) 1 45), (Point3D 12 0 2), (Point3D 6 5 4)] == Nothing

    print $ unsafeClosestTo (Point3D 2 5 10) [(Point3D 4 5 6), (Point3D 5 2 (-10)), (Point3D (-2) 1 45), (Point3D 12 0 2), (Point3D 6 5 4)] == [Point3D 4.0 5.0 6.0]

    print $ getClosestDistance [(Point3D 4 5 6), (Point3D 2 5 10), (Point3D 5 2 (-10)), (Point3D (-2) 1 45), (Point3D 12 0 2), (Point3D 6 5 4)] == Just (2.83, Point3D 4.0 5.0 6.0, Point3D 6.0 5.0 4.0)
    print $ getClosestDistance [(Point2D 2 3), (Point3D 3 4 5)] == Nothing

    print $ unsafeGetClosestDistance [(Point3D 4 5 6), (Point3D 2 5 10), (Point3D 5 2 (-10)), (Point3D (-2) 1 45), (Point3D 12 0 2), (Point3D 6 5 4)] == (2.83, Point3D 4.0 5.0 6.0, Point3D 6.0 5.0 4.0)

data Point a = Point2D a a | Point3D a a a
    deriving (Show, Eq)

roundTwoDig :: (Floating a, RealFrac a) => a -> a
roundTwoDig = (/ 100) . fromIntegral . round .(* 100)

getPoints :: (RealFrac a) => (a -> a) -> (a -> a) -> [Point a] -> [Point a]
getPoints f g = filter (\ (Point2D x y) -> f x == g y)

distance :: (RealFrac a, Floating a) => Point a -> Point a -> Maybe a
distance (Point2D x1 y1) (Point2D x2 y2) = Just $ roundTwoDig $ sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2
distance (Point3D x1 y1 z1) (Point3D x2 y2 z2) = Just $ roundTwoDig $ sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2
distance _ _ = Nothing

unsafeDistance :: (RealFrac a, Floating a) => Point a -> Point a -> a
unsafeDistance p1 p2 = case distance p1 p2 of
    Just d -> d
    Nothing -> error "Points have different number of dimensions"

closestTo :: (RealFrac a, Floating a) => Point a -> [Point a] -> Maybe [Point a]
closestTo _ [] = Just []
closestTo p ps = 
    case maybeMinDistance of
        Nothing -> Nothing
        minDistance -> Just $ filter (\ x -> distance p x == minDistance) ps
    where
        maybeMinDistance = 
            case catMaybes $ map (distance p) ps of
                [] -> Nothing
                ds -> Just $ minimum ds

unsafeClosestTo :: (RealFrac a, Floating a) => Point a -> [Point a] -> [Point a]
unsafeClosestTo p ps = let minDistance = minimum $ map (unsafeDistance p) ps 
    in filter (\ x -> unsafeDistance p x == minDistance) ps

getClosestDistance :: (RealFrac a, Floating a) => [Point a] -> Maybe (a, Point a, Point a)
getClosestDistance ps = 
    case triples of
        [] -> Nothing
        (x:xs) -> Just $ foldl (\ t1@(d1, _, _) t2@(d2, _, _) -> if (d2 < d1) then t2 else t1) x xs
    where
        certesianProduct = [ (x, y) | x <- ps, y <- ps, x /= y]
        triplesWithMaybes = map (\ (p1, p2) -> (distance p1 p2, p1, p2)) certesianProduct
        triplesWithoutNothings = filter (\ (maybeDistance, _, _) -> maybeDistance /= Nothing) triplesWithMaybes
        triples = map (\ ((Just d), p1, p2) -> (d, p1, p2)) triplesWithoutNothings

unsafeGetClosestDistance :: (RealFrac a, Floating a) => [Point a] -> (a, Point a, Point a)
unsafeGetClosestDistance ps = let ((p,q):zs) = [ (x, y) | x <- ps, y <- ps, x /= y]
    in foldl (\ acc@(d, p1, p2) (x, y) -> if (unsafeDistance x y < d) then (unsafeDistance x y, x, y) else acc) (unsafeDistance p q, p, q) zs
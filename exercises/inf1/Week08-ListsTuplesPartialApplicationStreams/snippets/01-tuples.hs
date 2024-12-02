myFirstHaskellTuple :: (Int, Int)
myFirstHaskellTuple = (2, 4)
-- std::pair<int, int>

moreComplexTuple :: (Int, [Double], Char)
moreComplexTuple = (2, [2.3, 4.6, 10.3], 'p')
-- std::tuple<int, std::vector<double>, char>

type Point2D = (Double, Double)
-- typedef std::pair<double, double> Point2D;

sumOfCoordinates :: Point2D -> Double
sumOfCoordinates (x, y) = x + y
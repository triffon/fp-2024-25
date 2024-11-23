module Tuple where

type Student = (String, Int, Double)

-- betterStudent :: Ord a1 => (a2, b, a1) -> (a2, b, a1) -> (a2, b, a1)
betterStudent :: Student -> Student -> Student
betterStudent s1@(_, _, grade1) s2@(_, _, grade2)
 | grade1 > grade2 = s1
 | otherwise       = s2
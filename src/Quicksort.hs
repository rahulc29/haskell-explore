module Quicksort where
qsort []       = []
qsort (x : xs) = qsort smallerElements ++ [x] ++ qsort largerElements
  where
    smallerElements = [ a | a <- xs, a < x ]
    largerElements  = [ b | b <- xs, b > x ]

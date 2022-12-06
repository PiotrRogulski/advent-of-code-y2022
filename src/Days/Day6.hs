module Days.Day6 (module Days.Day6) where

import Control.Lens ((<&>))
import Data.List (nub)
import Data.List.Split (divvy)
import DayInput (getDay)

firstIndexWhere :: (a -> Bool) -> [a] -> Int
firstIndexWhere predicate l = head [i | (el, i) <- zip l [0 ..], predicate el]

hasNoDuplicates :: Eq a => [a] -> Bool
hasNoDuplicates l = l == nub l

task :: Int -> IO Int
task n =
  getDay 6
    <&> divvy n 1
    <&> firstIndexWhere hasNoDuplicates
    <&> (+ n)

pt1 :: IO Int
pt1 = task 4

pt2 :: IO Int
pt2 = task 14

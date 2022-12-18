module Days.Day18 (module Days.Day18) where

import Control.Arrow (Arrow (second), (&&&), (***))
import Data.Functor ((<&>))
import DayInput (getDay)

type Coordinate = (Int, Int, Int)

input :: IO [Coordinate]
input =
  getDay 18
    <&> lines
    <&> map (("(" ++) . (++ ")"))
    <&> map (read @Coordinate)

findAdjacent :: [Coordinate] -> Coordinate -> [(Coordinate, Coordinate)]
findAdjacent points (x, y, z) =
  filter
    (`elem` points)
    [ (x + 1, y, z),
      (x - 1, y, z),
      (x, y + 1, z),
      (x, y - 1, z),
      (x, y, z + 1),
      (x, y, z - 1)
    ]
    <&> ((x, y, z),)

findAllAdjacent :: [Coordinate] -> [(Coordinate, Coordinate)]
findAllAdjacent [] = []
findAllAdjacent (p : ps) = findAdjacent ps p ++ findAllAdjacent ps

pt1 :: IO Int
pt1 =
  input
    <&> (length &&& id)
    <&> second findAllAdjacent
    <&> ((* 6) *** length)
    <&> second (* 2)
    <&> uncurry (-)

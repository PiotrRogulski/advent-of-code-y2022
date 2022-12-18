module Days.Day18 (module Days.Day18) where

import Control.Arrow (Arrow (second), (&&&), (***), (>>>))
import Control.Lens ((??))
import Data.Foldable (foldr')
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Set qualified as S
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

getBounds :: [Coordinate] -> (Coordinate, Coordinate)
getBounds points =
  let (x, y, z) = unzip3 points
   in ((minimum x - 1, minimum y - 1, minimum z - 1), (maximum x + 1, maximum y + 1, maximum z + 1))

isInBounds :: (Coordinate, Coordinate) -> Coordinate -> Bool
isInBounds ((x1, y1, z1), (x2, y2, z2)) (x, y, z) =
  x >= x1
    && x <= x2
    && y >= y1
    && y <= y2
    && z >= z1
    && z <= z2

neighbors :: Coordinate -> [Coordinate]
neighbors (x, y, z) =
  [ (x + 1, y, z),
    (x - 1, y, z),
    (x, y - 1, z),
    (x, y + 1, z),
    (x, y, z - 1),
    (x, y, z + 1)
  ]

walk :: S.Set Coordinate -> S.Set Coordinate -> Int -> (Coordinate, Coordinate) -> S.Set Coordinate -> Int
walk queue done acc bounds points
  | null queue = acc
  | otherwise =
      walk
        (foldr' S.insert queue' unexploredAir)
        (S.insert currentPoint done)
        (acc + length neighbouringLava)
        bounds
        points
  where
    (currentPoint, queue') = S.deleteFindMin queue
    unexploredAir =
      currentPoint
        & neighbors
        & filter
          ( ( [ not . (`S.member` points),
                not . (`S.member` done),
                isInBounds bounds
              ]
                ??
            )
              >>> and
          )
    neighbouringLava =
      currentPoint
        & neighbors
        & filter (`S.member` points)

theSearch :: [Coordinate] -> Int
theSearch points =
  let bounds = getBounds points
   in walk (S.singleton (fst bounds)) S.empty 0 bounds (S.fromList points)

pt2 :: IO Int
pt2 = input <&> theSearch

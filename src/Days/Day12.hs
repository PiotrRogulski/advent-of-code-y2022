module Days.Day12 (module Days.Day12) where

import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as HM
import DayInput (getDay)

newtype Graph a = Graph {unGraph :: [(a, [a])]} deriving (Show)

flattenIndexes :: (Int, [(Int, a)]) -> [((Int, Int), a)]
flattenIndexes (v, xs) = map (\(x, c) -> ((v, x), c)) xs

theMap :: IO (HM.HashMap (Int, Int) Char)
theMap =
  getDay 12
    <&> lines
    <&> map (zip [0 ..])
    <&> zip [0 ..]
    <&> concatMap flattenIndexes
    <&> HM.fromList

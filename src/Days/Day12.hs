{-# LANGUAGE LambdaCase #-}

module Days.Day12 (module Days.Day12) where

import Control.Arrow (Arrow (first, second), (>>>))
import Control.Lens ((&))
import Control.Lens.Combinators (_3)
import Control.Lens.Operators ((%~))
import Data.Char (ord)
import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.Maybe (fromJust, mapMaybe)
import Data.Tuple.HT (uncurry3)
import DayInput (getDay)

newtype Graph a = Graph {unGraph :: [(a, [a])]} deriving (Show)

dijkstra :: (Hashable a) => a -> a -> Graph a -> Int
dijkstra source target (Graph unGraph) = go (HM.singleton source 0) (HM.singleton source Nothing) [source]
  where
    go _ _ [] = error "No path found"
    go dists prevs (x : xs)
      | x == target = dist dists x
      | otherwise = go (HM.union dists newDists) (HM.union prevs newPrevs) (xs ++ newNodes)
      where
        newDists = HM.fromList $ map (,dist dists x + 1) (neighbors x)
        newPrevs = HM.fromList $ map (,Just x) (neighbors x)
        newNodes = filter (not . flip HM.member dists) (neighbors x)
        neighbors x' = fromJust $ lookup x' unGraph
    dist dists x = fromJust $ HM.lookup x dists

type Position = (Int, Int)

type MapGraph = Graph Position

parseLevel :: Char -> Int
parseLevel = \case
  'S' -> ord 'a'
  'E' -> ord 'z'
  s | s `elem` ['a' .. 'z'] -> ord s
  _ -> error "Invalid map level"

mapLevel :: HM.HashMap Position Char -> Position -> Maybe Int
mapLevel m pos = (HM.!?) m pos <&> parseLevel

flattenIndexes :: (Int, [(Int, a)]) -> [(Position, a)]
flattenIndexes (v, xs) = map (\(x, c) -> ((v, x), c)) xs

mapToGraph :: HM.HashMap Position Char -> MapGraph
mapToGraph m = Graph (HM.toList assocs)
  where
    assocs = HM.mapWithKey findNeighbors m
    findNeighbors :: Position -> Char -> [Position]
    findNeighbors pos level = okNeighbors <&> fst
      where
        okNeighbors = filter (snd >>> pred >>> (<= currLevel)) levelsAround
        currLevel = parseLevel level
        levelsAround =
          mapMaybe
            ( ($ pos) >>> (\p -> mapLevel m p <&> (p,))
            )
            [ second (+ 1),
              second (subtract 1),
              first (+ 1),
              first (subtract 1)
            ]

withStartEnd :: HM.HashMap Position Char -> (Position, Position, HM.HashMap Position Char)
withStartEnd m = (start, end, m)
  where
    start = HM.toList m & filter (\(_, c) -> c == 'S') & head & fst
    end = HM.toList m & filter (\(_, c) -> c == 'E') & head & fst

input :: IO (HM.HashMap Position Char)
input =
  getDay 12
    <&> lines
    <&> map (zip [0 ..])
    <&> zip [0 ..]
    <&> concatMap flattenIndexes
    <&> HM.fromList

pt1 :: IO Int
pt1 =
  input
    <&> withStartEnd
    <&> _3 %~ mapToGraph
    <&> uncurry3 dijkstra

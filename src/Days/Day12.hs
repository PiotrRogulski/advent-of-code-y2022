module Days.Day12 (module Days.Day12) where

import Control.Arrow (Arrow (first, second, (&&&)), (>>>))
import Control.Lens ((&))
import Control.Lens.Combinators (_2, _3)
import Control.Lens.Operators ((%~))
import Data.Char (ord)
import Data.Composition ((.:))
import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.List (foldl1')
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.Profunctor.Product.Flatten (flatten3)
import Data.Tuple.HT (uncurry3)
import DayInput (getDay)

newtype Graph a = Graph {unGraph :: [(a, [a])]} deriving (Show)

dijkstra :: (Hashable a) => a -> a -> Graph a -> Maybe Int
dijkstra source target (Graph unGraph) = go (HM.singleton source 0) (HM.singleton source Nothing) [source]
  where
    go _ _ [] = Nothing
    go dists prevs (x : xs)
      | x == target = Just $ dist dists x
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
parseLevel 'S' = ord 'a'
parseLevel 'E' = ord 'z'
parseLevel s
  | s `elem` ['a' .. 'z'] = ord s
  | otherwise = error $ "Invalid level: " ++ show s

mapLevel :: Position -> HM.HashMap Position Char -> Maybe Int
mapLevel = fmap parseLevel .: HM.lookup

flattenIndexes :: (Int, [(Int, a)]) -> [(Position, a)]
flattenIndexes (v, xs) = map (first (v,)) xs

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
            (($ pos) >>> (&&& (,)) (`mapLevel` m) >>> uncurry (<&>))
            ([second, first] <*> [subtract 1, (+ 1)])

findSymbol :: HM.HashMap Position Char -> Char -> [Position]
findSymbol m c = HM.toList m & filter ((== c) . snd) & map fst

withStartEnd :: HM.HashMap Position Char -> (Position, Position, HM.HashMap Position Char)
withStartEnd m = (start, end, m)
  where
    start = head $ findSymbol m 'S'
    end = head $ findSymbol m 'E'

withEnd :: HM.HashMap Position Char -> (Position, HM.HashMap Position Char)
withEnd m = (end, m)
  where
    end = head $ findSymbol m 'E'

withAllFound :: Char -> HM.HashMap Position Char -> ([Position], HM.HashMap Position Char)
withAllFound c = flip findSymbol c &&& id

input :: IO (HM.HashMap Position Char)
input =
  getDay 12
    <&> lines
    <&> map (zip [0 ..])
    <&> zip [0 ..]
    <&> concatMap flattenIndexes
    <&> HM.fromList

pt1 :: IO (Maybe Int)
pt1 =
  input
    <&> withStartEnd
    <&> _3 %~ mapToGraph
    <&> uncurry3 dijkstra

distributeSecond :: ([a], b) -> [(a, b)]
distributeSecond (xs, y) = [(x, y) | x <- xs]

pt2 :: IO Int
pt2 =
  input
    <&> withAllFound 'a'
    <&> _2 %~ withEnd
    <&> _2 %~ _2 %~ mapToGraph
    <&> distributeSecond
    <&> map (uncurry3 dijkstra . flatten3)
    <&> catMaybes
    <&> foldl1' min

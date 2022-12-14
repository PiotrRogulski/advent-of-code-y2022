module Days.Day14 (module Days.Day14) where

import Control.Arrow (Arrow ((&&&)), (>>>))
import Data.Foldable (minimumBy)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List (nub)
import Data.List.Split (divvy)
import Data.Ord (comparing)
import DayInput (getDay)
import GHC.List (iterate')
import Text.Parsec (char, digit, many1, parse, sepBy, string)
import Text.Parsec.String (Parser)

pairParser :: Parser (Int, Int)
pairParser = do
  x <- many1 digit <&> read
  _ <- char ','
  y <- many1 digit <&> read
  return (x, y)

pathParser :: Parser [(Int, Int)]
pathParser = pairParser `sepBy` string " -> "

pairsBetween :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
pairsBetween (x1, y1) (x2, y2)
  | x1 == x2 = [(x1, y) | y <- [min y1 y2 .. max y1 y2]]
  | y1 == y2 = [(x, y1) | x <- [min x1 x2 .. max x1 x2]]
  | otherwise = error "not on a line"

toPair :: [a] -> (a, a)
toPair [x, y] = (x, y)
toPair _ = error "not a pair"

pairsBetween' :: [(Int, Int)] -> [(Int, Int)]
pairsBetween' = divvy 2 1 >>> map toPair >>> map (uncurry pairsBetween) >>> concat

data MapTile = Sand | Rock deriving (Eq, Show)

type Map = HM.HashMap (Int, Int) MapTile

initialMap :: IO (HM.HashMap (Int, Int) MapTile)
initialMap =
  getDay 14
    <&> lines
    <&> map (parse pathParser "")
    <&> map (either (error . show) id)
    <&> map pairsBetween'
    <&> concat
    <&> nub
    <&> flip zip (repeat Rock)
    <&> HM.fromList

dropSand ::
  (Map -> (Int, Int) -> Maybe MapTile) ->
  (Map -> (Int, Int) -> Maybe ((Int, Int), MapTile)) ->
  (Int, Int) ->
  Map ->
  (Bool, Map)
dropSand getter finder source m =
  case finder m source of
    Nothing -> (False, m)
    Just ((px, py), _) ->
      let left = getter m (px - 1, py); right = getter m (px + 1, py)
       in case (left, right) of
            (Just _, Just _) -> (True, HM.insert (px, py - 1) Sand m)
            (Nothing, _) -> dropSand getter finder (px - 1, py) m
            (_, Nothing) -> dropSand getter finder (px + 1, py) m

safeMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
safeMinimumBy _ [] = Nothing
safeMinimumBy f xs = Just $ minimumBy f xs

findBelow1 :: Map -> (Int, Int) -> Maybe ((Int, Int), MapTile)
findBelow1 m (x, y) =
  m
    & HM.keysSet
    & HS.filter (\(x', y') -> x' == x && y' >= y)
    & HS.toList
    & safeMinimumBy (comparing snd)
    <&> (id &&& (m HM.!))

getAt1 :: Map -> (Int, Int) -> Maybe MapTile
getAt1 m (x, y) = m HM.!? (x, y)

sandSource :: (Int, Int)
sandSource = (500, 0)

pourSand1 :: Map -> Map
pourSand1 =
  (True,)
    >>> iterate' (dropSand getAt1 findBelow1 sandSource . snd)
    >>> dropWhile fst
    >>> head
    >>> snd

pt1 :: IO Int
pt1 =
  initialMap
    <&> pourSand1
    <&> HM.filter (== Sand)
    <&> HM.size

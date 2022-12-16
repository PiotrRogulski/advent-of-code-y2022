module Days.Day15 (module Days.Day15) where

import Control.Arrow (Arrow (second), first, (&&&), (***), (>>>))
import Control.Parallel.Strategies (NFData, parMap, rdeepseq)
import Data.Foldable (find)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (nub)
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import Data.Tuple (swap)
import DayInput (getDay)
import GHC.Generics (Generic)
import Text.Parsec (digit, many1, parse, string, (<|>))
import Text.Parsec.String (Parser)

type Position = (Int, Int)

data Reading = Reading {sensor :: Position, beacon :: Position} deriving (Show)

int :: Parser Int
int = do
  sign <- string "-" <|> string ""
  digits <- many1 digit
  return $ read $ sign ++ digits

readingParser :: Parser Reading
readingParser = do
  _ <- string "Sensor at x="
  sensorX <- int
  _ <- string ", y="
  sensorY <- int
  _ <- string ": closest beacon is at x="
  beaconX <- int
  _ <- string ", y="
  beaconY <- int
  return $ Reading (sensorX, sensorY) (beaconX, beaconY)

parseReading :: String -> Reading
parseReading = either (error . show) id . parse readingParser ""

manhattanDistance :: Position -> Position -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

readingDistance :: Reading -> Int
readingDistance (Reading sensor beacon) = manhattanDistance sensor beacon

readingScanRow :: Int -> Reading -> Maybe (Int, Int)
readingScanRow y reading@(Reading (sx, sy) _)
  | dist < abs (y - sy) = Nothing
  | otherwise = Just (sx - width, sx + width)
  where
    dist = readingDistance reading
    width = dist - abs (y - sy)

input :: IO [Reading]
input =
  getDay 15
    <&> lines
    <&> map parseReading

targetRow :: Int
targetRow = 2_000_000

data IntRange = IntRange Int Int deriving (Show, Generic)

instance NFData IntRange

range :: (Int, Int) -> IntRange
range = uncurry IntRange

inRange :: Int -> IntRange -> Bool
inRange x (IntRange a b) = a <= x && x <= b

inAnyRange :: Int -> [IntRange] -> Bool
inAnyRange x = any (inRange x)

rangesSpan :: [IntRange] -> Int
rangesSpan =
  concatMap (\(IntRange a b) -> [a, b])
    >>> (maximum &&& minimum)
    >>> uncurry (-)
    >>> succ

pt1 :: IO Int
pt1 =
  input
    <&> (id &&& map (readingScanRow targetRow))
    <&> map beacon *** catMaybes
    <&> second (map range)
    <&> first nub
    <&> first (filter ((== targetRow) . snd))
    <&> (\(bs, rs) -> (filter (`inAnyRange` rs) (map fst bs), rs))
    <&> first length
    <&> second rangesSpan
    <&> uncurry (flip (-))

searchMax :: Int
searchMax = 4_000_000

generateRanges :: Int -> [Reading] -> [[IntRange]]
generateRanges m rs = [map range (mapMaybe (readingScanRow y) rs) | y <- [0 .. m]]

simplifyRanges :: IntRange -> IntRange -> [IntRange]
simplifyRanges (IntRange a b) (IntRange c d)
  | a <= c && d <= b = [IntRange a b]
  | a > c && d > b = [IntRange c d]
  | a <= c && c <= b = [IntRange a (max b d)]
  | c <= a && a <= d = [IntRange c (max b d)]
  | otherwise = [IntRange a b, IntRange c d]

simplifyRanges' :: [IntRange] -> [IntRange]
simplifyRanges' = foldr f []
  where
    f (IntRange a b) [] = [IntRange a b]
    f (IntRange a b) (IntRange c d : rs) = simplifyRanges (IntRange a b) (IntRange c d) ++ rs

findNotCovered :: Int -> Int -> [IntRange] -> Maybe Int
findNotCovered from to rs = [from .. to] & find (not . flip inAnyRange rs)

pt2 :: IO Integer
pt2 =
  input
    <&> generateRanges searchMax
    <&> map simplifyRanges'
    <&> zip [0 ..]
    <&> parMap rdeepseq (second (id &&& findNotCovered 0 searchMax))
    <&> filter (isJust . snd . snd)
    <&> head
    <&> fst &&& fromJust . snd . snd
    <&> swap
    <&> first ((* 4_000_000) . fromIntegral)
    <&> uncurry (+)

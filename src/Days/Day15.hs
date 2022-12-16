module Days.Day15 (module Days.Day15) where

import Control.Arrow (Arrow (second), first, (&&&), (***), (>>>))
import Data.Functor ((<&>))
import Data.List (nub)
import Data.Maybe (catMaybes)
import DayInput (getDay)
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

data IntRange = IntRange Int Int deriving (Show)

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

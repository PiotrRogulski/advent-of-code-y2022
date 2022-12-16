module Days.Day15 (module Days.Day15) where

import Data.Functor ((<&>))
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

input :: IO [Reading]
input =
  getDay 15
    <&> lines
    <&> map parseReading

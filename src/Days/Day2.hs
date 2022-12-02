{-# LANGUAGE OverloadedRecordDot #-}

module Days.Day2 (module Days.Day2) where

import Data.Char (ord)
import Data.Functor ((<&>))
import DayInput (getDay)
import GHC.Char (chr)

data Shape = Rock | Paper | Scissors deriving (Eq, Show)

data Round = Round {opponent :: Shape, my :: Shape} deriving (Eq, Show)

shapeToIdx :: Shape -> Int
shapeToIdx Rock = 0
shapeToIdx Paper = 1
shapeToIdx Scissors = 2

idxToShape :: Int -> Shape
idxToShape 0 = Rock
idxToShape 1 = Paper
idxToShape 2 = Scissors
idxToShape _ = error "Invalid shape index"

charToShape :: Char -> Char -> Shape
charToShape base = idxToShape . flip (-) (ord base) . ord

parseOpponentShape :: Char -> Shape
parseOpponentShape = charToShape 'A'

parseMyShape :: Char -> Shape
parseMyShape = charToShape 'X'

parseLine :: String -> Round
parseLine [os, ' ', ms] = Round (parseOpponentShape os) (parseMyShape ms)
parseLine _ = error "Invalid line"

roundOutcome :: Round -> Int
roundOutcome r
  | idxDiff == 0 = 3
  | idxDiff == 1 = 0
  | idxDiff == 2 = 6
  | otherwise = error "Invalid shape index"
  where
    idxDiff = (shapeToIdx r.opponent - shapeToIdx r.my) `mod` 3

roundScore :: Round -> Int
roundScore r = roundOutcome r + shapeToIdx r.my + 1

calculateFinalScore :: (String -> Round) -> IO Int
calculateFinalScore parser =
  getDay 2
    <&> lines
    <&> map parser
    <&> map roundScore
    <&> sum

parseLine' :: String -> Round
parseLine' [os, ' ', outcome] = Round (parseOpponentShape os) (parseMyShape ms')
  where
    ms' = chr $ (ord os - ord 'A' + offset) `mod` 3 + ord 'X'
    offset = case outcome of
      'X' -> 2
      'Y' -> 0
      'Z' -> 1
      _ -> error "Invalid outcome"
parseLine' _ = error "Invalid line"

--------------------------------------------------------------------------------

pt1 :: IO Int
pt1 = calculateFinalScore parseLine

pt2 :: IO Int
pt2 = calculateFinalScore parseLine'
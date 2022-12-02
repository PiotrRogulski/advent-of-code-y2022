module Days.Day2 (module Days.Day2) where

import Data.Functor ((<&>))
import DayInput (getDay)

data Shape = Rock | Paper | Scissors deriving (Eq, Show)

newtype OpponentShape = OS Shape deriving (Eq, Show)

newtype MyShape = MS Shape deriving (Eq, Show)

parseOpponentShape :: Char -> OpponentShape
parseOpponentShape 'A' = OS Rock
parseOpponentShape 'B' = OS Paper
parseOpponentShape 'C' = OS Scissors
parseOpponentShape _ = error "Invalid opponent shape"

parseMyShape :: Char -> MyShape
parseMyShape 'X' = MS Rock
parseMyShape 'Y' = MS Paper
parseMyShape 'Z' = MS Scissors
parseMyShape _ = error "Invalid my shape"

data Round = Round OpponentShape MyShape deriving (Eq, Show)

parseLine :: String -> Round
parseLine [os, ' ', ms] = Round (parseOpponentShape os) (parseMyShape ms)
parseLine _ = error "Invalid line"

roundOutcome :: Round -> Int
roundOutcome (Round (OS Rock) (MS Paper)) = 6
roundOutcome (Round (OS Paper) (MS Scissors)) = 6
roundOutcome (Round (OS Scissors) (MS Rock)) = 6
roundOutcome (Round (OS Rock) (MS Scissors)) = 0
roundOutcome (Round (OS Paper) (MS Rock)) = 0
roundOutcome (Round (OS Scissors) (MS Paper)) = 0
roundOutcome _ = 3

roundScore :: Round -> Int
roundScore r@(Round _ ms) = roundOutcome r + myShapeScore ms
  where
    myShapeScore :: MyShape -> Int
    myShapeScore (MS Rock) = 1
    myShapeScore (MS Paper) = 2
    myShapeScore (MS Scissors) = 3

pt1 :: IO Int
pt1 = getDay 2 <&> lines <&> map parseLine <&> map roundScore <&> sum

---------------------------------------------------------------------

parseLine' :: String -> Round
parseLine' [os, ' ', outcome] = Round (parseOpponentShape os) (parseMyShape ms)
  where
    ms = case outcome of
      'X' -> case os of
        'A' -> 'Z'
        'B' -> 'X'
        'C' -> 'Y'
        _ -> error "Invalid opponent shape"
      'Y' -> case os of
        'A' -> 'X'
        'B' -> 'Y'
        'C' -> 'Z'
        _ -> error "Invalid opponent shape"
      'Z' -> case os of
        'A' -> 'Y'
        'B' -> 'Z'
        'C' -> 'X'
        _ -> error "Invalid opponent shape"
      _ -> error "Invalid outcome"
parseLine' _ = error "Invalid line"

pt2 :: IO Int
pt2 = getDay 2 <&> lines <&> map parseLine' <&> map roundScore <&> sum
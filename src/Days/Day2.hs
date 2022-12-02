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
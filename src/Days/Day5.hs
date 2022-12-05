module Days.Day5 (module Days.Day5) where

import Control.Lens (ix, (^.))
import Control.Lens.Combinators (_1, _2)
import Control.Lens.Operators ((%~))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (transpose)
import Data.List.Split (splitOn)
import DayInput (getDay)

data Command = Command
  { count :: Int,
    from :: Int,
    to :: Int
  }
  deriving (Show)

type Stacks = [[Char]]

filterByIndex :: (Int -> Bool) -> [a] -> [a]
filterByIndex predicate l = [el | (el, i) <- zip l [0 ..], predicate i]

parseCommand :: String -> Command
parseCommand s = Command {count, from = from - 1, to = to - 1}
  where
    (count, from, to) =
      splitOn " " s
        & filterByIndex odd
        & map read
        & toParts
    toParts [c, f, t] = (c, f, t)
    toParts _ = error "Invalid command"

parseStacks :: [[Char]] -> Stacks
parseStacks s =
  s
    & init
    & transpose
    & filterByIndex ((== 1) . (`mod` 4))
    & map (filter (/= ' '))

inputData :: IO (Stacks, [Command])
inputData =
  getDay 5
    <&> lines
    <&> splitOn [""]
    <&> separateParts
    <&> _1 %~ parseStacks
    <&> _2 %~ map parseCommand
  where
    separateParts [a, b] = (a, b)
    separateParts _ = error "Invalid input"

executeCommand :: Stacks -> Command -> Stacks
executeCommand stacks Command {count, from, to} =
  stacks
    & ix from %~ drop count
    & ix to %~ (elementToMove ++)
  where
    elementToMove = stacks ^. ix from & take count

expandCommand :: Command -> [Command]
expandCommand Command {count, from, to} = replicate count (Command 1 from to)

pt1 :: IO [Char]
pt1 =
  inputData
    <&> _2 %~ concatMap expandCommand
    <&> uncurry (foldl executeCommand)
    <&> map head

pt2 :: IO [Char]
pt2 =
  inputData
    <&> uncurry (foldl executeCommand)
    <&> map head

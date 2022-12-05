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
    (count, from, to) = toParts $ map (read @Int) $ filterByIndex odd $ splitOn " " s
    toParts [c, f, t] = (c, f, t)
    toParts _ = error "Invalid command"

parseStacks :: [[Char]] -> Stacks
parseStacks s =
  s
    & init
    & transpose
    & filterByIndex (\i -> i `mod` 4 == 1)
    & map (filter (/= ' '))

inputData :: IO (Stacks, [Command])
inputData = do
  parts <- getDay 5 <&> lines <&> splitOn [""] <&> separateParts
  return $ parts & _1 %~ parseStacks & _2 %~ map parseCommand
  where
    separateParts [a, b] = (a, b)
    separateParts _ = error "Invalid input"

executeCommand :: Stacks -> Command -> Stacks
executeCommand stacks Command {count, from, to} =
  stacks & (ix from %~ drop count) & (ix to %~ (++) elementToMove)
  where
    elementToMove = stacks ^. ix from & take count

expandCommand :: Command -> [Command]
expandCommand Command {count, from, to} = [Command 1 from to | _ <- [1 .. count]]

pt1 :: IO Stacks
pt1 = inputData <&> _2 %~ concatMap expandCommand <&> uncurry (foldl executeCommand)

pt2 :: IO Stacks
pt2 = inputData <&> uncurry (foldl executeCommand)

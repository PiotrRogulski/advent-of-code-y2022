module Days.Day10 (module Days.Day10) where

import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (isPrefixOf, scanl')
import Data.List.Split (chunksOf)
import DayInput (getDay)

data Command = Noop | AddX Int deriving (Show, Eq)

parseCommand :: String -> Command
parseCommand "noop" = Noop
parseCommand cmd
  | "addx " `isPrefixOf` cmd = cmd & drop 5 & read & AddX
  | otherwise = error "Invalid input"

expandCommand :: Command -> [Command]
expandCommand Noop = [Noop]
expandCommand (AddX n) = [Noop, AddX n]

executeCommand :: Int -> Command -> Int
executeCommand n Noop = n
executeCommand n (AddX m) = n + m

input :: IO [Int]
input =
  getDay 10
    <&> lines
    <&> map parseCommand
    <&> concatMap expandCommand
    <&> scanl' executeCommand 1

pickSignals :: [Int] -> [Int] -> [Int]
pickSignals idx =
  zip [1 ..]
    >>> filter ((`elem` idx) . fst)
    >>> map (uncurry (*))

pt1 :: IO Int
pt1 =
  input
    <&> pickSignals [20, 60 .. 220]
    <&> sum

screenWidth :: Int
screenWidth = 40

charForSignalInTime :: Int -> Int -> Char
charForSignalInTime t n
  | abs (n - (t `mod` screenWidth)) <= 1 = '#'
  | otherwise = '.'

pt2 :: IO ()
pt2 =
  input
    >>= ( zipWith charForSignalInTime [0 ..]
            >>> chunksOf screenWidth
            >>> mapM_ putStrLn
        )

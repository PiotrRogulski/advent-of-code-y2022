{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Days.Day4 (module Days.Day4) where

import Data.Functor ((<&>))
import Data.List.Split (splitOneOf)
import DayInput (getDay)

type LineMatch = (String, String, String, [String])

data Range = Range Int Int deriving (Show)

isSubsetOf :: Range -> Range -> Bool
(Range a b) `isSubsetOf` (Range c d) = a >= c && b <= d

hasOverlap :: Range -> Range -> Bool
r1 `hasOverlap` r2 = r1 `isSubsetOf` r2 || r2 `isSubsetOf` r1

parseLine :: String -> (Range, Range)
parseLine s = (Range (read a) (read b), Range (read c) (read d))
  where
    [a, b, c, d] = splitOneOf ",-" s

pt1 :: IO Int
pt1 =
  getDay 4
    <&> lines
    <&> map parseLine
    <&> filter (uncurry hasOverlap)
    <&> length

module Days.Day3 (module Days.Day3) where

import Data.Char (ord)
import Data.Functor ((<&>))
import Data.List (intersect, nub)
import DayInput (getDay)

itemValue :: Char -> Int
itemValue c
  | c `elem` ['a' .. 'z'] = ord c - ord 'a' + 1
  | c `elem` ['A' .. 'Z'] = ord c - ord 'A' + 27
  | otherwise = error "Invalid item"

splitInHalf :: [a] -> ([a], [a])
splitInHalf xs = splitAt half xs
  where
    half = length xs `div` 2

pt1 :: IO Int
pt1 =
  getDay 3
    <&> lines
    <&> map splitInHalf
    <&> map (uncurry intersect)
    <&> map nub
    <&> map (sum . map itemValue)
    <&> sum

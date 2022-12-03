module Days.Day3 (module Days.Day3) where

import Data.Char (ord)
import Data.Functor ((<&>))
import Data.List (intersect, nub)
import Data.List.Split (chunksOf)
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

intersect3 :: Eq a => [[a]] -> [a]
intersect3 [a, b, c] = a `intersect` b `intersect` c
intersect3 _ = error "Invalid input"

------------------------------------------------------------

pt1 :: IO Int
pt1 =
  getDay 3
    <&> lines
    <&> map splitInHalf
    <&> map (uncurry intersect)
    <&> map nub
    <&> map (sum . map itemValue)
    <&> sum

pt2 :: IO Int
pt2 =
  getDay 3
    <&> lines
    <&> chunksOf 3
    <&> map intersect3
    <&> map nub
    <&> map (sum . map itemValue)
    <&> sum

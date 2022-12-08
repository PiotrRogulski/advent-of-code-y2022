module Days.Day8 (module Days.Day8) where

import Control.Arrow (Arrow ((***)))
import Data.Char (ord)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (transpose)
import Data.List.HT (takeUntil)
import DayInput (getDay)

getTreesInCrosshair :: [[Int]] -> Int -> Int -> ([Int], [Int], [Int], [Int])
getTreesInCrosshair trees' x y = (left, right, top, bottom)
  where
    (left, right) = splitter y (trees' !! x)
    (top, bottom) = splitter x (transpose trees' !! y)
    splitter = ((reverse *** drop 1) .) . splitAt

isTreeVisible :: [[Int]] -> Int -> Int -> Bool
isTreeVisible trees' x y = any (all (< tree)) lists
  where
    tree = trees' !! x !! y
    lists = [left, right, top, bottom]
    (left, right, top, bottom) = getTreesInCrosshair trees' x y

scenicScore :: [[Int]] -> Int -> Int -> Int
scenicScore trees' x y = lists & map length & product
  where
    tree = trees' !! x !! y
    lists = map (takeUntil (>= tree)) [left, right, top, bottom]
    (left, right, top, bottom) = getTreesInCrosshair trees' x y

trees :: IO [[Int]]
trees =
  getDay 8
    <&> lines
    <&> map (map (flip (-) (ord '0') . ord))

treesSize :: [[Int]] -> (Int, Int)
treesSize trees' = (width, height)
  where
    width = length $ head trees'
    height = length trees'

mapTrees :: ([[Int]] -> Int -> Int -> a) -> [[Int]] -> [a]
mapTrees f trees' =
  [ f trees' x y
    | x <- [0 .. height - 1],
      y <- [0 .. width - 1]
  ]
  where
    (width, height) = treesSize trees'

pt1 :: IO Int
pt1 = trees <&> length . filter id . mapTrees isTreeVisible

pt2 :: IO Int
pt2 = trees <&> maximum . mapTrees scenicScore

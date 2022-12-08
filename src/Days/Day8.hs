module Days.Day8 (module Days.Day8) where

import Control.Arrow (Arrow (second, (***)))
import Data.Char (ord)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (transpose)
import Data.List.HT (takeUntil)
import DayInput (getDay)

isTreeVisible :: [[Int]] -> Int -> Int -> Bool
isTreeVisible trees' x y = any (all (< tree)) lists
  where
    tree = trees' !! x !! y
    lists = [left, right, top, bottom]
    (left, right) = second (drop 1) $ splitAt y (trees' !! x)
    (top, bottom) = second (drop 1) $ splitAt x (transpose trees' !! y)

scenicScore :: [[Int]] -> Int -> Int -> Int
scenicScore trees' x y = lists & map length & product
  where
    tree = trees' !! x !! y
    lists = map (takeUntil (>= tree)) [left, right, top, bottom]
    (left, right) = (reverse *** drop 1) $ splitAt y (trees' !! x)
    (top, bottom) = (reverse *** drop 1) $ splitAt x (transpose trees' !! y)

trees :: IO [[Int]]
trees =
  getDay 8
    <&> lines
    <&> map (map (flip (-) (ord '0') . ord))

processTrees1 :: [[Int]] -> Int
processTrees1 trees' =
  length
    [ ()
      | x <- [0 .. height - 1],
        y <- [0 .. width - 1],
        isTreeVisible trees' x y
    ]
  where
    width = length $ head trees'
    height = length trees'

processTrees2 :: [[Int]] -> Int
processTrees2 trees' =
  [ scenicScore trees' x y
    | x <- [0 .. height - 1],
      y <- [0 .. width - 1]
  ]
    & maximum
  where
    width = length $ head trees'
    height = length trees'

pt1 :: IO Int
pt1 = trees <&> processTrees1

pt2 :: IO Int
pt2 = trees <&> processTrees2

module Days.Day8 (module Days.Day8) where

import Control.Arrow (Arrow (second))
import Data.Char (ord)
import Data.Functor ((<&>))
import Data.List (transpose)
import DayInput (getDay)

isTreeVisible :: [[Int]] -> Int -> Int -> Bool
isTreeVisible trees' x y = any (all (< tree)) lists
  where
    tree = trees' !! x !! y
    lists = [left, right, top, bottom]
    (left, right) = second (drop 1) $ splitAt y (trees' !! x)
    (top, bottom) = second (drop 1) $ splitAt x (transpose trees' !! y)

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

pt1 :: IO Int
pt1 = trees <&> processTrees1

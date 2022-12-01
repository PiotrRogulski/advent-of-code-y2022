{-# LANGUAGE OverloadedStrings #-}

module Days.Day1 (module Days.Day1) where

import Data.Functor ((<&>))
import Data.Text (Text, pack, splitOn, unpack)
import DayInput (getDay)
import Data.List (sortBy)

sums :: IO [Int]
sums = getDay 1 <&> splitOn ("\n\n" :: Text) . pack <&> map unpack <&> map lines <&> map (map (read @Int)) <&> map sum

pt1 :: IO Int
pt1 = sums <&> foldl max 0

pt2 :: IO Int
pt2 = sums <&> sortBy (flip compare) <&> take 3 <&> sum
{-# LANGUAGE OverloadedStrings #-}

module Days.Day1 (module Days.Day1) where

import Data.Functor ((<&>))
import Data.Text (Text, pack, splitOn, unpack)
import DayInput (getDay)

pt1 :: IO Int
pt1 = getDay 1 <&> splitOn ("\n\n" :: Text) . pack <&> map unpack <&> map lines <&> map (map (read @Int)) <&> map sum <&> foldl max 0

pt2 :: IO ()
pt2 = do
  putStrLn "part 2"
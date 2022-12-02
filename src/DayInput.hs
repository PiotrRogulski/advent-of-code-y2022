module DayInput (getDay) where

getDay :: Int -> IO String
getDay n = readFile $ "data/input" ++ show n ++ ".txt"

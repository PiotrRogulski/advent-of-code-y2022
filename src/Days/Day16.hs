module Days.Day16 (module Days.Day16) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import DayInput (getDay)
import Text.Parsec (anyChar, choice, count, digit, many1, parse, sepBy, string, try)
import Text.Parsec.String (Parser)

data Graph k v = Graph
  { nodes :: HM.HashMap k v,
    edges :: HM.HashMap k [k]
  }
  deriving (Show, Eq)

data ValveInfo = ValveInfo
  { name :: String,
    flowRate :: Int,
    tunnels :: [String]
  }
  deriving (Show, Eq)

type VolcanoGraph = Graph String ValveInfo

valveInfoParser :: Parser ValveInfo
valveInfoParser = do
  _ <- string "Valve "
  name <- count 2 anyChar
  _ <- string " has flow rate="
  flowRate <- many1 digit <&> read
  _ <- choice $ map (try . string) ["; tunnels lead to valves ", "; tunnel leads to valve "]
  tunnels <- count 2 anyChar `sepBy` string ", "
  return
    ValveInfo
      { name = name,
        flowRate = flowRate,
        tunnels = tunnels
      }

parseValveInfo :: String -> ValveInfo
parseValveInfo = either (error . show) id . parse valveInfoParser ""

valveInfoToGraph :: [ValveInfo] -> VolcanoGraph
valveInfoToGraph valveInfo =
  Graph
    { nodes = valveInfo & map (\vi -> (name vi, vi)) & HM.fromList,
      edges = HM.fromList $ map (\vi -> (name vi, tunnels vi)) valveInfo
    }

theGraph :: IO VolcanoGraph
theGraph =
  getDay 16
    <&> lines
    <&> map parseValveInfo
    <&> valveInfoToGraph

startValve :: String
startValve = "AA"

walk :: Int -> Int -> String -> HS.HashSet String -> VolcanoGraph -> Int
walk mLeft total start visited g
  | mLeft == 0 = total
  | otherwise =
      let next = HM.lookupDefault [] start (edges g)
       in maximum $
            map
              ( \s ->
                  walk
                    (mLeft - 1)
                    ( total
                        + ( if HS.member s visited
                              then flowRate (nodes g HM.! start) * mLeft
                              else 0
                          )
                    )
                    s
                    (HS.insert s visited)
                    g
              )
              next

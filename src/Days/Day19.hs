{-# LANGUAGE TemplateHaskell #-}

module Days.Day19 (module Days.Day19) where

import Control.Lens (makeLenses, (+~), (-~), (??), (^.))
import Data.Function ((&))
import Data.Functor ((<&>))
import DayInput (getDay)
import Text.Parsec (digit, many1, parse, string)
import Text.Parsec.String (Parser)

data Blueprint = Blueprint
  { _index :: Int,
    _oreRobotCost :: Int,
    _clayRobotCost :: Int,
    _obsidianRobotCost :: (Int, Int),
    _geodeRobotCost :: (Int, Int)
  }
  deriving (Show, Eq)

makeLenses ''Blueprint

blueprintParser :: Parser Blueprint
blueprintParser = do
  _ <- string "Blueprint "
  index <- read <$> many1 digit
  _ <- string ": Each ore robot costs "
  oreRobotCost <- read <$> many1 digit
  _ <- string " ore. Each clay robot costs "
  clayRobotCost <- read <$> many1 digit
  _ <- string " ore. Each obsidian robot costs "
  obsidianRobotOreCost <- read <$> many1 digit
  _ <- string " ore and "
  obsidianRobotClayCost <- read <$> many1 digit
  _ <- string " clay. Each geode robot costs "
  geodeRobotOreCost <- read <$> many1 digit
  _ <- string " ore and "
  geodeRobotObsidianCost <- read <$> many1 digit
  _ <- string " obsidian."
  return
    Blueprint
      { _index = index,
        _oreRobotCost = oreRobotCost,
        _clayRobotCost = clayRobotCost,
        _obsidianRobotCost = (obsidianRobotOreCost, obsidianRobotClayCost),
        _geodeRobotCost = (geodeRobotOreCost, geodeRobotObsidianCost)
      }

parseBlueprint :: String -> Blueprint
parseBlueprint = either (error . show) id . parse blueprintParser ""

data FactoryState = FactoryState
  { _ore :: Int,
    _clay :: Int,
    _obsidian :: Int,
    _geode :: Int,
    _oreRobots :: Int,
    _clayRobots :: Int,
    _obsidianRobots :: Int,
    _geodeRobots :: Int
  }
  deriving (Show, Eq)

makeLenses ''FactoryState

initialState :: FactoryState
initialState =
  FactoryState
    { _ore = 0,
      _clay = 0,
      _obsidian = 0,
      _geode = 0,
      _oreRobots = 1,
      _clayRobots = 0,
      _obsidianRobots = 0,
      _geodeRobots = 0
    }

(~/) :: Int -> Int -> Int
(~/) = div

infixl 6 ~/

waitMinute :: Blueprint -> FactoryState -> FactoryState
waitMinute blueprint state =
  state''''
    & ore +~ state ^. oreRobots
    & clay +~ state ^. clayRobots
    & obsidian +~ state ^. obsidianRobots
    & geode +~ state ^. geodeRobots
  where
    geodeRobotsToBuild =
      min
        (state ^. ore ~/ (blueprint ^. geodeRobotCost & fst))
        (state ^. obsidian ~/ (blueprint ^. geodeRobotCost & snd))
    geodeRobotsOreCost = geodeRobotsToBuild * (blueprint ^. geodeRobotCost & fst)
    geodeRobotsObsidianCost = geodeRobotsToBuild * (blueprint ^. geodeRobotCost & snd)
    state' =
      state
        & geodeRobots +~ geodeRobotsToBuild
        & obsidian -~ geodeRobotsObsidianCost
        & ore -~ geodeRobotsOreCost
    obsidianRobotsToBuild =
      min
        (state' ^. ore ~/ (blueprint ^. obsidianRobotCost & fst))
        (state' ^. clay ~/ (blueprint ^. obsidianRobotCost & snd))
    obsidianRobotsOreCost = obsidianRobotsToBuild * (blueprint ^. obsidianRobotCost & fst)
    obsidianRobotsClayCost = obsidianRobotsToBuild * (blueprint ^. obsidianRobotCost & snd)
    state'' =
      state'
        & obsidianRobots +~ obsidianRobotsToBuild
        & clay -~ obsidianRobotsClayCost
        & ore -~ obsidianRobotsOreCost
    clayRobotsToBuild = state'' ^. ore ~/ blueprint ^. clayRobotCost
    clayRobotsOreCost = clayRobotsToBuild * blueprint ^. clayRobotCost
    state''' =
      state''
        & clayRobots +~ clayRobotsToBuild
        & ore -~ clayRobotsOreCost
    oreRobotsToBuild = state''' ^. ore ~/ blueprint ^. oreRobotCost
    oreRobotsOreCost = oreRobotsToBuild * blueprint ^. oreRobotCost
    state'''' =
      state'''
        & oreRobots +~ oreRobotsToBuild
        & ore -~ oreRobotsOreCost

waitForFactory :: FactoryState -> Int -> Blueprint -> FactoryState
waitForFactory state 0 _ = state
waitForFactory state n blueprint = waitForFactory (waitMinute blueprint state) (n - 1) blueprint

input :: IO [Blueprint]
input =
  getDay 19
    <&> lines
    <&> map parseBlueprint

pt1 :: IO FactoryState
pt1 =
  input
    <&> map (map (waitForFactory initialState) [0 .. 20] ??)
    <&> head
    <&> last

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Days.Day17 (module Days.Day17) where

import Control.Applicative ((<|>))
import Control.Arrow (Arrow (first, second, (***)))
import Control.Lens (makeLenses, (%~), (+~), (.~), (??), (^.), _2)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashSet qualified as HS
import Data.Ord (clamp)
import DayInput (getDay)

data Direction
  = L
  | R
  deriving (Show, Eq)

directionChange :: Direction -> Int
directionChange = \case
  L -> -1
  R -> 1

parseDirection :: Char -> Direction
parseDirection '<' = L
parseDirection '>' = R
parseDirection _ = error "Invalid direction"

data Shape
  = Horizontal
  | Plus
  | Jay
  | Vertical
  | Square
  deriving (Show, Eq)

shapeHeight :: Shape -> Int
shapeHeight = \case
  Horizontal -> 1
  Plus -> 3
  Jay -> 3
  Vertical -> 4
  Square -> 2

shapePoints :: (Int, Int) -> Shape -> [(Int, Int)]
shapePoints (x, y) = \case
  Horizontal -> [(+ 0), (+ 1), (+ 2), (+ 3)] <&> second ?? (x, y)
  Plus -> [id, first succ, first pred, second succ, second pred] ?? (x, y)
  Jay -> [id, second (+ 1), second (+ 2), (+ 1) *** (+ 2), (+ 2) *** (+ 2)] ?? (x, y)
  Vertical -> [(+ 0), (+ 1), (+ 2), (+ 3)] <&> first ?? (x, y)
  Square -> [id, first succ, second succ, succ *** succ] ?? (x, y)

shapes :: [Shape]
shapes = cycle [Horizontal, Plus, Jay, Vertical, Square]

data TickPhase
  = JetPhase
  | FallPhase
  deriving (Show, Eq)

data BoardState = BoardState
  { _board :: HS.HashSet (Int, Int),
    _width :: Int,
    _shapeTick :: Int,
    _jetTick :: Int,
    _currentShape :: Shape,
    _shapePosition :: (Int, Int),
    _jetQueue :: [Direction],
    _shapeQueue :: [Shape],
    _phase :: TickPhase
  }
  deriving (Eq)

makeLenses ''BoardState

instance Show BoardState where
  show s =
    unlines $
      [ "BoardState {",
        "  board = " <> show (s ^. board),
        "  width = " <> show (s ^. width),
        "  shapeTick = " <> show (s ^. shapeTick),
        "  jetTick = " <> show (s ^. jetTick),
        "  currentShape = " <> show (s ^. currentShape),
        "  shapePosition = " <> show (s ^. shapePosition),
        "  jetQueue = " <> show (s ^. jetQueue & take 3) <> ", ...",
        "  shapeQueue = " <> show (s ^. shapeQueue & take 3) <> ", ...",
        "  phase = " <> show (s ^. phase),
        "}"
      ]

getColumnTops :: BoardState -> [Int]
getColumnTops s = [0 .. s ^. width - 1] <&> getTop
  where
    getTop :: Int -> Int
    getTop x =
      s ^. board
        & HS.filter ((== x) . snd)
        & HS.map fst
        & HS.toList
        & (<|> [0])
        & maximum

shapeTouchesTops :: BoardState -> Bool
shapeTouchesTops s = any (== 0) $ getColumnTops s

jetPattern :: IO [Direction]
jetPattern =
  getDay 17
    <&> init
    <&> map parseDirection
    <&> cycle

initialState :: [Direction] -> BoardState
initialState pat =
  BoardState
    { _board = HS.empty,
      _width = 7,
      _shapeTick = 0,
      _jetTick = 0,
      _currentShape = Horizontal,
      _shapePosition = (3, 2),
      _jetQueue = pat,
      _shapeQueue = shapes,
      _phase = JetPhase
    }

performJetPhase :: BoardState -> BoardState
performJetPhase s =
  s
    & jetTick +~ 1
    & jetQueue %~ tail
    & phase .~ FallPhase
    & shapePosition . _2 %~ moveShape
  where
    moveShape :: Int -> Int
    moveShape y = clamp (0, s ^. width - 1) $ y + directionChange (s ^. jetQueue & head)

performFallPhase :: BoardState -> BoardState
performFallPhase s =
  s
    & phase .~ JetPhase
    & undefined

performTick :: BoardState -> BoardState
performTick s = case s ^. phase of
  JetPhase -> performJetPhase s
  FallPhase -> performFallPhase s

pt1 = jetPattern <&> initialState <&> iterate performTick <&> take 5

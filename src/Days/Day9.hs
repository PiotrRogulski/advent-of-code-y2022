{-# LANGUAGE TemplateHaskell #-}

module Days.Day9 (module Days.Day9) where

import Control.Lens (makeLenses, (%~), (^.))
import Control.Lens.Combinators (ix)
import Control.Lens.Operators ((.~))
import Data.Foldable (Foldable (foldl'))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (nub)
import DayInput (getDay)

data Direction = R | D | L | U deriving (Show, Eq, Read)

data MultiCommand = MultiCommand Int Direction deriving (Show, Eq)

parseMultiCommand :: String -> MultiCommand
parseMultiCommand (d : ' ' : n) = MultiCommand (read n) (read [d])
parseMultiCommand _ = error "Invalid input"

expandMultiCommand :: MultiCommand -> [Direction]
expandMultiCommand (MultiCommand n d) = replicate n d

type Position = (Int, Int)

data Rope = Rope {_theRope :: [Position], _visited :: [Position]} deriving (Show, Eq)

makeLenses ''Rope

makeRope :: Int -> Rope
makeRope n = Rope (replicate n (0, 0)) [(0, 0)]

moveHead :: Direction -> Position -> Position
moveHead d (x, y) = case d of
  R -> (x + 1, y)
  D -> (x, y - 1)
  L -> (x - 1, y)
  U -> (x, y + 1)

moveLink ::
  Position -> -- head
  Position -> -- tail
  Position
moveLink h@(hx, hy) t@(tx, ty)
  | h == t = t
  | abs (hx - tx) <= 1 && abs (hy - ty) <= 1 = t
  | hx == tx = (tx, ty + signum (hy - ty))
  | hy == ty = (tx + signum (hx - tx), ty)
  | otherwise = (tx + signum (hx - tx), ty + signum (hy - ty))

moveRope :: Rope -> Direction -> Rope
moveRope r d = r'
  where
    r' = r & theRope .~ newRope & visited %~ (last newRope :)

    newHead = r ^. theRope & head & moveHead d

    newRope = newHead : moveRopeInner (r ^. theRope & (ix 0 .~ newHead))

    moveRopeInner :: [Position] -> [Position]
    moveRopeInner [] = error "Empty rope"
    moveRopeInner [_] = []
    moveRopeInner (x : y : xs) = y' : moveRopeInner (y' : xs)
      where
        y' = moveLink x y

input :: IO [Direction]
input =
  getDay 9
    <&> lines
    <&> map parseMultiCommand
    <&> concatMap expandMultiCommand

pt1 :: IO Int
pt1 =
  input
    <&> foldl' moveRope (makeRope 2)
    <&> (visited %~ nub)
    <&> (^. visited)
    <&> length

pt2 :: IO Int
pt2 =
  input
    <&> foldl' moveRope (makeRope 10)
    <&> (visited %~ nub)
    <&> (^. visited)
    <&> length

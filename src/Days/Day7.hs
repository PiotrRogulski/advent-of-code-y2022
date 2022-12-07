{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Days.Day7 (module Days.Day7) where

import Control.Arrow ((&&&))
import Control.Lens (filtered, (%~), (.~), (^.))
import Control.Lens.Combinators (_1, _2)
import Control.Lens.Operators ((&))
import Control.Lens.TH (makeLenses)
import Data.Foldable (foldl', minimumBy)
import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import DayInput (getDay)

data FsFile = FsFile String Int deriving (Show, Eq)

data Fs = Leaf FsFile | Node {_name :: String, _items :: [Fs]} deriving (Show, Eq)

makeLenses ''Fs

nodeHasName :: String -> Fs -> Bool
nodeHasName _ (Leaf _) = False
nodeHasName n (Node nodeName _) = nodeName == n

insert :: [String] -> Fs -> Fs -> Fs
insert [] nodeToInsert node@(Node _ children) =
  if nodeToInsert `elem` children
    then node
    else node & items %~ (++ [nodeToInsert])
insert (p : ps) nodeToInsert node@(Node _ _)
  | any (nodeHasName p) (node ^. items) = node & items %~ (traverse . filtered (nodeHasName p) %~ insert ps nodeToInsert)
  | otherwise = node & items %~ (++ [insert ps nodeToInsert (Node p [])])
insert _ _ (Leaf _) = error "Invalid node"

getNodeSize :: Fs -> Int
getNodeSize (Leaf (FsFile _ size)) = size
getNodeSize (Node _ children) = sum $ map getNodeSize children

nodeWithSize :: Fs -> (Fs, Int)
nodeWithSize = id &&& getNodeSize

getAllNodes :: Fs -> [Fs]
getAllNodes (Leaf _) = []
getAllNodes (Node _ children) = children ++ concatMap getAllNodes children

isDirWithSize :: (Int -> Bool) -> Fs -> Bool
isDirWithSize _ (Leaf _) = False
isDirWithSize sizePred node = sizePred $ getNodeSize node

data ParseState = ParseState {_pwd :: [String], _fs :: Fs} deriving (Show)

makeLenses ''ParseState

data OutputEntry
  = Cd String
  | CdUp
  | CdRoot
  | FileEntry String Int
  | DirEntry String
  deriving (Show)

parseLine :: String -> OutputEntry
parseLine "$ cd .." = CdUp
parseLine "$ cd /" = CdRoot
parseLine line
  | "$ cd " `isPrefixOf` line = Cd $ drop 5 line
  | "dir " `isPrefixOf` line = DirEntry $ drop 4 line
  | otherwise = uncurry FileEntry fileParts
  where
    fileParts = toParts $ splitOn " " line
    toParts [size, n] = (n, read size)
    toParts _ = error "Invalid line"

executeLine :: ParseState -> OutputEntry -> ParseState
executeLine st e = case e of
  Cd dir -> st & pwd %~ (++ [dir])
  CdUp -> st & pwd %~ init
  CdRoot -> st & pwd .~ []
  FileEntry fileName size -> st & fs %~ insert (st ^. pwd) (Leaf (FsFile fileName size))
  DirEntry dirName -> st & fs %~ insert (st ^. pwd) (Node dirName [])

reducer :: ParseState -> String -> ParseState
reducer st line = line & parseLine & executeLine st

input :: IO Fs
input =
  getDay 7
    <&> lines
    <&> filter (/= "$ ls")
    <&> foldl' reducer (ParseState [] (Node "/" []))
    <&> (^. fs)

pt1 :: IO Int
pt1 =
  input
    <&> getAllNodes
    <&> filter (isDirWithSize (< 100_000))
    <&> map getNodeSize
    <&> sum

pt2 :: IO Int
pt2 =
  input
    <&> nodeWithSize
    <&> _2 %~ (70_000_000 -)
    <&> _1 %~ getAllNodes
    <&> _1 %~ filter (isDirWithSize (> 100_000))
    <&> _1 %~ map (id &&& getNodeSize)
    <&> (\(nodes, freeSpace) -> filter ((> 30_000_000) . (+ freeSpace) . snd) nodes)
    <&> minimumBy (comparing snd)
    <&> snd

module Days.Day7 (module Days.Day7) where

import Control.Lens.Operators ((&))
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import DayInput (getDay)
import Debug.Trace (trace)

data FsFile = FsFile String Int deriving (Show, Eq)

data Fs = Leaf FsFile | Node String [Fs] deriving (Show, Eq)

nodeHasName :: String -> Fs -> Bool
nodeHasName _ (Leaf _) = False
nodeHasName name (Node n _) = n == name

insert :: [String] -> Fs -> Fs -> Fs
insert [] nodeToInsert node@(Node n children) = if node `elem` children then node else Node n $ children ++ [nodeToInsert]
insert (p : ps) nodeToInsert (Node n children)
  | any (nodeHasName p) children = Node n $ map (\c -> if nodeHasName p c then insert ps nodeToInsert c else c) children
  | otherwise = Node n $ children ++ [insert ps nodeToInsert (Node p [])]
insert _ _ (Leaf _) = error "Invalid node"

data ParseState = ParseState {pwd :: [String], fs :: Fs} deriving (Show)

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
    toParts [size, name] = (name, read size)
    toParts _ = error "Invalid line"

executeLine :: ParseState -> OutputEntry -> ParseState
executeLine (ParseState pwd fs) e | trace ("\n### executeLine (" ++ show pwd ++ ") (" ++ show fs ++ ") (" ++ show e ++ ")") False = undefined
executeLine (ParseState pwd fs) e = case e of
  Cd dir -> ParseState (pwd ++ [dir]) fs
  CdUp -> ParseState (init pwd) fs
  CdRoot -> ParseState [] fs
  FileEntry name size -> ParseState pwd $ insert pwd (Leaf (FsFile name size)) fs
  DirEntry name -> ParseState pwd $ insert pwd (Node name []) fs

reducer :: ParseState -> String -> ParseState
reducer (ParseState pwd fs) line = line & parseLine & executeLine (ParseState pwd fs)

input :: IO ParseState
input =
  getDay 7
    <&> lines
    <&> filter (/= "$ ls")
    <&> foldl' reducer (ParseState [] (Node "/" []))


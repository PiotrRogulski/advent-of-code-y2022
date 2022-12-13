module Days.Day13 (module Days.Day13) where

import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Control.Lens (both)
import Control.Lens.Operators ((%~))
import Data.Functor ((<&>))
import Data.List (sort)
import Data.List.Split (splitOn)
import DayInput (getDay)
import Text.Parsec (digit, many1, parse, sepBy)
import Text.Parsec.Char (char)
import Text.Parsec.String (Parser)

input :: IO [[String]]
input =
  getDay 13
    <&> lines
    <&> splitOn [""]

data Tree a = Leaf a | Node [Tree a] deriving (Eq)

instance Show a => Show (Tree a) where
  show :: Tree a -> String
  show (Leaf a) = show a
  show (Node a) = show a

instance Ord a => Ord (Tree a) where
  compare :: Tree a -> Tree a -> Ordering
  compare (Leaf a) (Leaf b) = compare a b
  compare l@(Leaf _) n@(Node _) = compare (Node [l]) n
  compare n@(Node _) l@(Leaf _) = compare n (Node [l])
  compare (Node a) (Node b)
    | null a && null b = EQ
    | null a = LT
    | null b = GT
    | otherwise = compare (head a) (head b) <> compare (tail a) (tail b)

leafParser :: Parser (Tree Int)
leafParser =
  many1 digit
    <&> read
    <&> Leaf

nodeParser :: Parser (Tree Int)
nodeParser =
  char '['
    >> sepBy treeParser (char ',')
    >>= \children ->
      char ']'
        >> return (Node children)

treeParser :: Parser (Tree Int)
treeParser = leafParser <|> nodeParser

parseTree :: String -> Tree Int
parseTree = parse treeParser "" >>> either (error . show) id

listToPair :: [a] -> (a, a)
listToPair [a, b] = (a, b)
listToPair _ = error "List must have length 2"

pt1 :: IO Int
pt1 =
  input
    <&> map listToPair
    <&> map (both %~ parseTree)
    <&> map (uncurry compare)
    <&> zip [1 ..]
    <&> filter ((<= EQ) . snd)
    <&> map fst
    <&> sum

marker1 :: Tree Int
marker1 = parseTree "[[2]]"

marker2 :: Tree Int
marker2 = parseTree "[[6]]"

pt2 :: IO Int
pt2 =
  input
    <&> concat
    <&> map parseTree
    <&> (++ [marker1, marker2])
    <&> sort
    <&> zip [1 ..]
    <&> filter ((`elem` [marker1, marker2]) . snd)
    <&> map fst
    <&> product

{-# LANGUAGE TemplateHaskell #-}

module Days.Day11 (module Days.Day11) where

import Control.Arrow ((&&&))
import Control.Lens (makeLenses, (^?!), _Right)
import Control.Lens.Operators ((^.))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.Split (splitOn)
import DayInput (getDay)
import Text.Parsec (ParseError, anyChar, char, many1, manyTill, newline, oneOf, parse, space, string)
import Text.Parsec.Char (digit)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec ((<|>))

data Monkey = Monkey
  { _items :: [Int],
    _operation :: Int -> Int,
    _action :: Int -> Int
  }

instance Show Monkey where
  show m =
    "Monkey with "
      ++ show (m & _items & length)
      ++ " elements: "
      ++ show (m & _items)

makeLenses ''Monkey

parseList :: Parser [Int]
parseList = do
  vals <- many1 (many1 digit <|> string ", ")
  return $ vals & filter (/= ", ") & map read

parseOperation :: Parser (Int -> Int)
parseOperation = do
  _ <- string "  Operation: new = old "
  op <- oneOf "+*"
  _ <- space
  arg <- manyTill anyChar (char '\n')
  opFun <- case op of
    '+' -> return (+)
    '*' -> return (*)
    _ -> error "Invalid operation"
  return $ case arg of
    "old" -> uncurry opFun . (id &&& id)
    v -> opFun (read v)

parseAction :: Parser (Int -> Int)
parseAction = do
  _ <- string "  Test: divisible by "
  val <- many1 digit
  _ <- newline
  _ <- string "    If true: throw to monkey "
  m1 <- many1 digit
  _ <- newline
  _ <- string "    If false: throw to monkey "
  m2 <- many1 digit
  _ <- newline
  return $ \n ->
    if n `mod` read val == 0
      then read m1
      else read m2

parseMonkey :: Parser Monkey
parseMonkey = do
  _ <- string "Monkey "
  _ <- many1 digit
  _ <- string ":"
  _ <- newline
  _ <- string "  Starting items: "
  vals <- parseList
  _ <- newline
  op <- parseOperation
  act <- parseAction
  return Monkey {_items = vals, _operation = op, _action = act}

input :: IO [Monkey]
input =
  getDay 11
    <&> lines
    <&> splitOn [""]
    <&> map unlines
    <&> map (parse parseMonkey "input11.txt")
    <&> map (^?! _Right)

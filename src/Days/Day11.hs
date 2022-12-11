{-# LANGUAGE TemplateHaskell #-}

module Days.Day11 (module Days.Day11) where

import Control.Arrow (Arrow (first, (***)), (&&&))
import Control.Lens (makeLenses, (+~), (.~), (^?!), _Right)
import Control.Lens.Combinators (_1, _2)
import Control.Lens.Operators ((%~), (^.))
import Data.Composition ((.:))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (partition, sortBy)
import Data.List.Split (splitOn)
import DayInput (getDay)
import Text.Parsec (anyChar, char, many1, manyTill, newline, oneOf, parse, space, string)
import Text.Parsec.Char (digit)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec ((<|>))

data Monkey = Monkey
  { _items :: [Int],
    _operation :: Int -> Int,
    _action :: Int -> Int,
    _count :: Int
  }

makeLenses ''Monkey

instance Show Monkey where
  show Monkey {_items = its, _count = cnt} =
    ""
      ++ show (its & length)
      ++ ": "
      ++ show its
      ++ " "
      ++ show cnt

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
  return
    Monkey
      { _items = vals,
        _operation = op,
        _action = act,
        _count = 0
      }

type ExchangeItem =
  ( Int, -- target Monkey ID
    Int -- item to exchange
  )

processMonkey :: Monkey -> [ExchangeItem]
processMonkey m = zip targets newItems
  where
    newItems = m ^. items & map ((`div` 3) . (m ^. operation))
    targets = newItems & map (m ^. action)

exchangeItems :: [Monkey] -> [ExchangeItem]
exchangeItems = concatMap processMonkey

updateMonkeysWithItems :: [Monkey] -> [ExchangeItem] -> [Monkey]
updateMonkeysWithItems ms its =
  [ m
      & items
        %~ ( <> map snd (filter ((== i) . fst) its)
           )
    | (m, i) <- zip ms [0 ..]
  ]

processMonkeys :: [Monkey] -> [Monkey]
processMonkeys ms = updateMonkeysWithItems monkeys' leftovers
  where
    (monkeys', leftovers) = updateMonkey 0 ms

    updateMonkey :: Int -> [Monkey] -> ([Monkey], [ExchangeItem])
    updateMonkey _ [] = ([], [])
    updateMonkey idx (monkey : monkeys) =
      updateMonkey (succ idx) (updateMonkeysWithItems monkeys forwardItems)
        & _1 %~ (monkey' :)
        & _2 %~ (backwardItems ++)
      where
        (forwardItems, backwardItems) = partition ((> idx) . fst) (processMonkey monkey) & _1 %~ map (first (subtract (idx + 1)))
        monkey' = monkey & items .~ [] & count +~ (monkey ^. items & length)

f :: Monkey -> Monkey -> (Int, Int)
f = curry ((^. count) *** (^. count))

g :: (Int, Int) -> Ordering
g = uncurry (flip compare)

h :: Monkey -> Monkey -> Ordering
h = g .: f

monkeyBusiness :: [Monkey] -> Int
monkeyBusiness ms = ms & sortBy h & take 2 & map (^. count) & product

input :: IO [Monkey]
input =
  getDay 11
    <&> lines
    <&> splitOn [""]
    <&> map unlines
    <&> map (parse parseMonkey "input11.txt")
    <&> map (^?! _Right)

pt1 =
  input
    <&> iterate processMonkeys
    <&> take 21
    <&> last
    <&> monkeyBusiness

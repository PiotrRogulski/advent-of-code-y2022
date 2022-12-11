{-# LANGUAGE TemplateHaskell #-}

module Days.Day11 (module Days.Day11) where

import Control.Arrow (Arrow (first, (***)), (&&&), (>>>))
import Control.Lens (makeLenses, (+~), (.~), (^?!), _Right)
import Control.Lens.Combinators (_1, _2)
import Control.Lens.Operators ((%~), (^.))
import Data.Composition ((.:))
import Data.Foldable (foldr')
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (partition, sortBy)
import Data.List.Split (splitOn)
import DayInput (getDay)
import Text.Parsec (anyChar, char, many1, manyTill, newline, oneOf, parse, space, string)
import Text.Parsec.Char (digit)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec ((<|>))

type ItemValue = Integer

data Monkey = Monkey
  { _items :: [ItemValue],
    _operation :: ItemValue -> ItemValue,
    _action :: ItemValue -> Int,
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

parseList :: Parser [ItemValue]
parseList = do
  vals <- many1 (many1 digit <|> string ", ")
  return $ vals & filter (/= ", ") & map read

parseOperation :: Parser (ItemValue -> ItemValue)
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

parseAction :: Parser (ItemValue -> Int)
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
    ItemValue -- item to exchange
  )

processMonkey1 :: Monkey -> [ExchangeItem]
processMonkey1 m = zip targets newItems
  where
    newItems = m ^. items & map ((`div` 3) . (m ^. operation))
    targets = newItems & map (m ^. action)

processMonkey2 :: Monkey -> [ExchangeItem]
processMonkey2 m = zip targets newItems
  where
    newItems = m ^. items & map (m ^. operation)
    targets = newItems & map (m ^. action)

updateMonkeysWithItems :: [Monkey] -> [ExchangeItem] -> [Monkey]
updateMonkeysWithItems ms its =
  [ m
      & items
        %~ ( <> map snd (filter ((== i) . fst) its)
           )
    | (m, i) <- zip ms [0 ..]
  ]

processMonkeys :: (Monkey -> [ExchangeItem]) -> [Monkey] -> [Monkey]
processMonkeys mProc ms = updateMonkeysWithItems monkeys' leftovers
  where
    (monkeys', leftovers) = updateMonkey 0 ms

    updateMonkey :: Int -> [Monkey] -> ([Monkey], [ExchangeItem])
    updateMonkey _ [] = ([], [])
    updateMonkey idx (monkey : monkeys) =
      updateMonkey (succ idx) (updateMonkeysWithItems monkeys forwardItems)
        & _1 %~ (monkey' :)
        & _2 %~ (backwardItems ++)
      where
        (forwardItems, backwardItems) = partition ((> idx) . fst) (mProc monkey) & _1 %~ map (first (subtract (idx + 1)))
        monkey' = monkey & items .~ [] & count +~ (monkey ^. items & length)

monkeyBusiness :: [Monkey] -> Int
monkeyBusiness =
  sortBy (uncurry (flip compare) .: curry ((^. count) *** (^. count)))
    >>> take 2
    >>> map (^. count)
    >>> product

input :: IO [Monkey]
input =
  getDay 11
    <&> lines
    <&> splitOn [""]
    <&> map unlines
    <&> map (parse parseMonkey "input11.txt")
    <&> map (^?! _Right)

pt1 :: IO Int
pt1 =
  input
    <&> flip (foldr' ($)) (replicate 20 (processMonkeys processMonkey1))
    <&> monkeyBusiness

pt2 :: IO Int
pt2 =
  input
    <&> flip (foldr' ($)) (replicate 10000 (processMonkeys processMonkey2))
    <&> monkeyBusiness

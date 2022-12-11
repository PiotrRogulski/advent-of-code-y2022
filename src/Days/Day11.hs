{-# LANGUAGE TemplateHaskell #-}

module Days.Day11 (module Days.Day11) where

import Control.Arrow (Arrow (first, (***)), (&&&), (>>>))
import Control.Lens (makeLenses, (+~), (.~), (^?!), _Right)
import Control.Lens.Combinators (_1, _2)
import Control.Lens.Operators ((%~), (^.))
import Data.Bool.HT (if')
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

type ItemValue = Int

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
parseAction =
  string "  Test: divisible by "
    >> many1 digit
    >>= \val ->
      newline
        >> string "    If true: throw to monkey "
        >> many1 digit
        >>= \m1 ->
          newline
            >> string "    If false: throw to monkey "
            >> many1 digit
            >>= \m2 ->
              newline
                >> return (\n -> read $ if' (n `mod` read val == 0) m1 m2)

parseMonkey :: Parser Monkey
parseMonkey =
  string "Monkey "
    >> many1 digit
    >> string ":"
    >> newline
    >> string "  Starting items: "
    >> parseList
    >>= \vals ->
      newline
        >> parseOperation
        >>= \op ->
          parseAction
            >>= \act ->
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

ringValue :: Int
ringValue = product [2, 3, 5, 7, 11, 13, 17, 19]

processMonkey :: (Int -> Int) -> Monkey -> [ExchangeItem]
processMonkey f m =
  m ^. items
    & map ((`mod` ringValue) . f . (m ^. operation))
    & ((map (m ^. action) &&& id) >>> uncurry zip)

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
        monkey' =
          monkey
            & items .~ []
            & count +~ (monkey ^. items & length)

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

process :: Int -> (Monkey -> [ExchangeItem]) -> [Monkey] -> [Monkey]
process s mProc ms = foldr' ($) ms (replicate s (processMonkeys mProc))

pt1 :: IO Int
pt1 =
  input
    <&> process 20 (processMonkey (`div` 3))
    <&> monkeyBusiness

pt2 :: IO Int
pt2 =
  input
    <&> process 10000 (processMonkey id)
    <&> monkeyBusiness

module Days.Day21 (module Days.Day21) where

import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as HM
import Data.List.Split (splitOn)
import DayInput (getDay)

data Expr
  = Const Int
  | Add Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Sub Expr Expr
  deriving (Show, Eq)

evalExpr :: Expr -> Int
evalExpr (Const n) = n
evalExpr (Add e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (Mul e1 e2) = evalExpr e1 * evalExpr e2
evalExpr (Div e1 e2) = evalExpr e1 `div` evalExpr e2
evalExpr (Sub e1 e2) = evalExpr e1 - evalExpr e2

isNumeric :: String -> Bool
isNumeric = all (`elem` ['0' .. '9'])

exprToWords :: String -> (String, String, String)
exprToWords s | [x, y, z] <- words s = (x, y, z)
exprToWords _ = error "exprToWords: invalid number of elements"

buildExpr :: String -> HM.HashMap String String -> Expr
buildExpr root m
  | isNumeric exprString = Const $ read exprString
  | otherwise = case op of
      "+" -> Add (buildExpr arg1 m) (buildExpr arg2 m)
      "*" -> Mul (buildExpr arg1 m) (buildExpr arg2 m)
      "/" -> Div (buildExpr arg1 m) (buildExpr arg2 m)
      "-" -> Sub (buildExpr arg1 m) (buildExpr arg2 m)
      _ -> error "buildExpr: invalid operator"
  where
    exprString = m HM.! root
    (arg1, op, arg2) = exprToWords exprString

toPair :: [a] -> (a, a)
toPair [x, y] = (x, y)
toPair _ = error "toPair: invalid number of elements"

input :: IO Expr
input =
  getDay 21
    <&> lines
    <&> map (splitOn ": ")
    <&> map toPair
    <&> HM.fromList
    <&> buildExpr "root"

pt1 :: IO Int
pt1 = input <&> evalExpr

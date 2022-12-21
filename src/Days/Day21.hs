module Days.Day21 (module Days.Day21) where

import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as HM
import Data.List.Split (splitOn)
import DayInput (getDay)

data BinOp = BinOp String (Int -> Int -> Int)

instance Show BinOp where
  show (BinOp s _) = s

instance Eq BinOp where
  (BinOp s1 _) == (BinOp s2 _) = s1 == s2

data Expr
  = Const Int
  | Op BinOp Expr Expr
  | Var String
  deriving (Eq)

instance Show Expr where
  show (Const n) = show n
  show (Op op e1 e2) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
  show (Var s) = "$" ++ s

evalExpr :: Expr -> Maybe Int
evalExpr (Const n) = Just n
evalExpr (Op (BinOp _ op) e1 e2) = op <$> evalExpr e1 <*> evalExpr e2
evalExpr (Var _) = Nothing

isNumeric :: String -> Bool
isNumeric = all (`elem` ['0' .. '9'])

exprToWords :: String -> (String, String, String)
exprToWords s | [x, y, z] <- words s = (x, y, z)
exprToWords _ = error "exprToWords: invalid number of elements"

buildExpr :: String -> [String] -> Bool -> HM.HashMap String String -> Expr
buildExpr root vars eqRoot m
  | root `elem` vars = Var root
  | isNumeric exprString = Const $ read exprString
  | eqRoot = Op (BinOp "-" (-)) (buildExpr arg1 vars False m) (buildExpr arg2 vars False m)
  | otherwise =
      Op
        ( uncurry BinOp $ case op of
            "+" -> ("+", (+))
            "*" -> ("*", (*))
            "/" -> ("/", div)
            "-" -> ("-", (-))
            _ -> error "buildExpr: invalid operator"
        )
        (buildExpr arg1 vars False m)
        (buildExpr arg2 vars False m)
  where
    exprString = m HM.! root
    (arg1, op, arg2) = exprToWords exprString

toPair :: [a] -> (a, a)
toPair [x, y] = (x, y)
toPair _ = error "toPair: invalid number of elements"

input :: IO (HM.HashMap [Char] [Char])
input =
  getDay 21
    <&> lines
    <&> map (splitOn ": ")
    <&> map toPair
    <&> HM.fromList

pt1 :: IO (Maybe Int)
pt1 =
  input
    <&> buildExpr "root" [] False
    <&> evalExpr

solveExpr :: Int -> String -> Expr -> Int
solveExpr _ _ (Const _) = error "solveExpr: expression is a constant"
solveExpr _ _ (Var _) = error "solveExpr: expression is an unbound variable"
solveExpr target var (Op (BinOp label _) left right) =
  case (valLeft, valRight) of
    (Just _, Just _) -> error "solveExpr: expression is fully bound"
    (Nothing, Nothing) -> error "solveExpr: variable on both sides of equality"
    (Just l, Nothing) -> case right of
      Var v ->
        if v /= var
          then error "solveExpr: found a different variable"
          else case label of
            "+" -> target - l
            "-" -> l - target
            "*" -> target `div` l
            "/" -> l `div` target
            _ -> error "solveExpr: invalid operator"
      Op {} -> case label of
        "+" -> solveExpr (target - l) var right
        "-" -> solveExpr (l - target) var right
        "*" -> solveExpr (target `div` l) var right
        "/" -> solveExpr (l `div` target) var right
        _ -> error "solveExpr: invalid operator"
      _ -> error "solveExpr: invalid expression"
    (Nothing, Just r) -> case left of
      Var v ->
        if v /= var
          then error "solveExpr: found a different variable"
          else case label of
            "+" -> target - r
            "-" -> target + r
            "*" -> target `div` r
            "/" -> target * r
            _ -> error "solveExpr: invalid operator"
      Op {} -> case label of
        "+" -> solveExpr (target - r) var left
        "-" -> solveExpr (target + r) var left
        "*" -> solveExpr (target `div` r) var left
        "/" -> solveExpr (target * r) var left
        _ -> error "solveExpr: invalid operator"
      _ -> error "solveExpr: invalid expression"
  where
    valLeft = evalExpr left
    valRight = evalExpr right

pt2 :: IO Int
pt2 =
  input
    <&> buildExpr "root" ["humn"] True
    <&> solveExpr 0 "humn"

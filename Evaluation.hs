module Evaluation (
  unwordsList,
  evaluating,
  readExpr
  ) where

import LispValue
import Parsing
import qualified Text.ParserCombinators.Parsec as P

readExpr :: String -> LispVal
readExpr input = case P.parse parseExpr "lisp" input of
  Left err -> String ("No match: " ++ show err)
  Right val -> val

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) (lookup func primitives)

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinaryOp (+)),
             ("-", numericBinaryOp (-)),
             ("*", numericBinaryOp (*)),
             ("/", numericBinaryOp div),
             ("mod", numericBinaryOp mod),
             ("quotient", numericBinaryOp quot),
             ("remainder", numericBinaryOp rem)]

numericBinaryOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinaryOp op operands = Number (foldl1 op (map unpackNumber operands))

evaluating :: LispVal -> LispVal
evaluating val@(String _) = val
evaluating val@(Number _) = val
evaluating val@(Bool _) = val
evaluating (List [Atom "quote", val]) = val
evaluating (List (Atom func : args)) = apply func (map evaluating args)


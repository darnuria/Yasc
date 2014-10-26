module Evaluation (
  evaluating,
  readExpr
  ) where

import LispValue
import LispError
import Parsing
import Control.Monad.Error

import qualified Text.ParserCombinators.Parsec as P

unpackNumber :: LispVal -> ThrowsError Integer
unpackNumber (Number n) = return n
unpackNumber (String n) =
    let parsed = reads n in
      if null parsed
        then throwError (TypeMismatch "number" (String n))
        else return (fst (head parsed))
unpackNumber (List [n]) = unpackNumber n
unpackNumber notNum = throwError (TypeMismatch "number" notNum)

readExpr :: String -> ThrowsError LispVal
readExpr input = case P.parse parseExpr "lisp" input of
  Left err -> throwError (Parser err)
  Right val -> return val

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  let msg = "Unrecognized primitive function arguments"
  in maybe (throwError $ NotFunction msg func)
    ($ args)
    (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinaryOp (+)),
             ("-", numericBinaryOp (-)),
             ("*", numericBinaryOp (*)),
             ("/", numericBinaryOp div),
             ("mod", numericBinaryOp mod),
             ("quotient", numericBinaryOp quot),
             ("remainder", numericBinaryOp rem)]

numericBinaryOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinaryOp _ [] = throwError (NumArgs 2 [])
numericBinaryOp _ singleVal@[_] = throwError (NumArgs 2 singleVal)
numericBinaryOp op operands = liftM (Number . foldl1 op) (mapM unpackNumber operands)

evaluating :: LispVal -> ThrowsError LispVal
evaluating val@(String _) = return val
evaluating val@(Number _) = return val
evaluating val@(Bool _) = return val
evaluating (List [Atom "quote", val]) = return val
evaluating (List (Atom func : args)) = mapM evaluating args >>= apply func
evaluating badForm = throwError (BadSpecialForm "Unrecognized special form" badForm)

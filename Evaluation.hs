module Evaluation (
  evaluating,
  readExpr
  ) where

import LispValue
import LispError
import Parsing
import Control.Monad.Error
import Unpacker
import Variables

import qualified Text.ParserCombinators.Parsec as P

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

evaluating :: Env -> LispVal -> IOThrowsError LispVal
evaluating _ val@(String _) = return val
evaluating _ val@(Number _) = return val
evaluating _ val@(Bool _) = return val
evaluating env (Atom tag) = getVar env tag
evaluating _ (List [Atom "quote", val]) = return val
evaluating env (List [Atom "if", predicate, conseq, alt]) =
  do result <- evaluating env predicate
     case result of
       Bool False -> evaluating env alt
       Bool True -> evaluating env conseq
evaluating env (List [Atom "set!", Atom var, form]) =
  evaluating env form >>= setVar env var
evaluating env (List [Atom "define", Atom var, form]) =
  evaluating env form >>= defineVar env var
evaluating env (List (Atom func : args)) =
  mapM (evaluating env) args >>= liftThrows . apply func
evaluating _ badForm =
  throwError (BadSpecialForm "Unrecognized special form" badForm)

-- Primitives --

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinaryOp (+)),
             ("-", numericBinaryOp (-)),
             ("*", numericBinaryOp (*)),
             ("/", numericBinaryOp div),
             ("mod", numericBinaryOp mod),
             ("quotient", numericBinaryOp quot),
             ("remainder", numericBinaryOp rem),
             ("=", numBoolBinaryOp (==)),
             ("/=", numBoolBinaryOp (/=)),
             ("<", numBoolBinaryOp (<)),
             (">", numBoolBinaryOp (>)),
             (">=", numBoolBinaryOp (>=)),
             ("<=", numBoolBinaryOp (<=)),
             ("||", logicBoolBinaryOp (||)),
             ("&&", logicBoolBinaryOp (&&)),
             ("=", numBoolBinaryOp (>=)),
             ("string=?", strBoolBinaryOp (==)),
             ("string<?", strBoolBinaryOp (<)),
             ("string>?", strBoolBinaryOp (>)),
             ("string<=?", strBoolBinaryOp (<=)),
             ("string>=?", strBoolBinaryOp (>=)),
             ("car", car),
             ("cdr", cdr),
             ("cons", cons),
             ("eq?", eqv),
             ("eqv?", eqv),
             ("equal?", equal)]

numBoolBinaryOp :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinaryOp = boolBinaryOp unpackNumber

strBoolBinaryOp :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinaryOp = boolBinaryOp unpackString

logicBoolBinaryOp :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
logicBoolBinaryOp = boolBinaryOp unpackBool

boolBinaryOp :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinaryOp unpacker op operands =
  if length operands /= 2
    then throwError (NumArgs 2 operands)
    else do
      left <- unpacker (head operands)
      right <- unpacker (operands !! 1)
      return (Bool (left `op` right))

numericBinaryOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinaryOp _ [] = throwError (NumArgs 2 [])
numericBinaryOp _ singleVal@[_] = throwError (NumArgs 2 singleVal)
numericBinaryOp op operands = liftM (Number . foldl1 op) (mapM unpackNumber operands)

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError (TypeMismatch "pair" badArg)
car badArgList = throwError (NumArgs 1 badArgList)

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return (List xs)
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return (DottedList xs x)
cdr [badArg] = throwError (TypeMismatch "pair" badArg)
cdr badArgList = throwError (NumArgs 1 badArgList)

cons :: [LispVal] -> ThrowsError LispVal
cons [x , List []] = return (List [x])
cons [x, List xs] = return (List (x : xs))
cons [x, DottedList xs xlast] = return (DottedList (x : xs) xlast)
cons [x1, x2] = return (DottedList [x1] x2)
cons badArgList = throwError (NumArgs 2 badArgList)

eqv :: [LispVal] -> Either LispError LispVal
eqv [(Bool arg1), (Bool arg2)] = return (Bool (arg1 == arg2))
eqv [(Number arg1), (Number arg2)] = return (Bool (arg1 == arg2))
eqv [(Atom arg1), (Atom arg2)] = return (Bool (arg1 == arg2))
eqv [(String arg1), (String arg2)] = return (Bool (arg1 == arg2))
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List (xs ++ [x]), List (ys ++ [y])]
eqv [(List arg1), (List arg2)] =
  return (Bool ((length arg1 == length arg2) &&
                all eqvPair (zip arg1 arg2)))
  where eqvPair (x1, x2) = case eqv [x1, x2] of
                             Left _ -> False
                             Right (Bool val) -> val
eqv [_, _] = return (Bool False)
eqv badArgList = throwError (NumArgs 2 badArgList)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- liftM or (mapM (unpackEquals arg1 arg2) unpackers)
  eqvEquals <- eqv [arg1, arg2]
  return (Bool (primitiveEquals || let (Bool x) = eqvEquals in x))
  where unpackers = [AnyUnpacker unpackNumber,
                    AnyUnpacker unpackString,
                    AnyUnpacker unpackBool]
equal badArgList = throwError (NumArgs 2 badArgList)


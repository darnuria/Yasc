module Evaluation (
  evaluating
  , readExpr
  , readExprList
  , primitiveBindings
  ) where

import Control.Monad.Error
import Data.Maybe(isNothing)
import System.IO

import qualified Text.ParserCombinators.Parsec as P

import Lisp
import Parsing
import Unpacker

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (P.endBy parseExpr P.spaces)

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (IOFunc func) args        = func args
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && isNothing varargs
    then throwError $ NumArgs (num params) args
    else liftIO (bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (evaluating env) body
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
          Nothing -> return env

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO (hClose port >> (return (Bool True)))
closePort _           = return (Bool False)


readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO (hGetLine port)) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [object] = writeProc [object, Port stdout]
writeProc [object, Port port] = liftIO (hPrint port object >> (return (Bool True)))

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List (load filename)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String (liftIO (readFile filename))

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO (readFile filename)) >>= liftThrows . readExprList

makeFunc :: (Monad m, Show a) => Maybe String -> Env -> [a] -> [LispVal] -> m LispVal
makeFunc varargs env params' body' =
  return $ Func (map show params') varargs body' env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> ErrorT LispError IO LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> ErrorT LispError IO LispVal
makeVarArgs = makeFunc . Just . show

evaluating :: Env -> LispVal -> IOThrowsError LispVal
evaluating _ val@(String _) = return val
evaluating _ val@(Number _) = return val
evaluating _ val@(Bool _)   = return val
evaluating env (Atom tag)   = getVar env tag
evaluating _ (List [Atom "quote", val]) = return val
evaluating env (List [Atom "if", predicate, conseq, alt]) =
  do result <- evaluating env predicate
     case result of
       Bool False -> evaluating env alt
       Bool True -> evaluating env conseq
evaluating env (List [Atom "load", String filename]) =
  load filename >>= liftM last . mapM (evaluating env)
evaluating env (List [Atom "set!", Atom var, form]) =
  evaluating env form >>= setVar env var
evaluating env (List [Atom "define", Atom var, form]) =
  evaluating env form >>= defineVar env var
evaluating env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
evaluating env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var
evaluating env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
evaluating env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarArgs varargs env params body
evaluating env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarArgs varargs env [] body
evaluating env (List (function : args)) = do
  func <- evaluating env function
  argVals <- mapM (evaluating env) args
  apply func argVals
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

primitiveBindings :: IO Env
primitiveBindings =
  let ioFunctions = map (makeFunc IOFunc) ioPrimitives
      functions   = map (makeFunc PrimitiveFunc) primitives
  in nullEnv >>= (flip bindVars $ ioFunctions ++ functions)
  where makeFunc constructor (var, func) = (var, constructor func)

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-file", closePort),
                ("close-output-file", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

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
eqv [Bool arg1, Bool arg2]             = return (Bool (arg1 == arg2))
eqv [Number arg1, Number arg2]         = return (Bool (arg1 == arg2))
eqv [Atom arg1, Atom arg2]             = return (Bool (arg1 == arg2))
eqv [String arg1, String arg2]         = return (Bool (arg1 == arg2))
eqv [DottedList xs x, DottedList ys y] = eqv [List (xs ++ [x]), List (ys ++ [y])]
eqv [List arg1, List arg2] =
  return (Bool ((length arg1 == length arg2) &&
                all eqvPair (zip arg1 arg2)))
  where eqvPair (x1, x2) = case eqv [x1, x2] of
                             Left _ -> False
                             Right (Bool val) -> val
eqv [_, _]                             = return (Bool False)
eqv badArgList                         = throwError (NumArgs 2 badArgList)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- liftM or (mapM (unpackEquals arg1 arg2) unpackers)
  eqvEquals <- eqv [arg1, arg2]
  return (Bool (primitiveEquals || let (Bool x) = eqvEquals in x))
  where unpackers = [AnyUnpacker unpackNumber,
                    AnyUnpacker unpackString,
                    AnyUnpacker unpackBool]
equal badArgList = throwError (NumArgs 2 badArgList)


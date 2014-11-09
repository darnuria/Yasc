module Lisp (
  LispVal(
    Atom
    , List
    , DottedList
    , Number
    , String
    , Bool
    , Func
    , IOFunc
    , Port
    , PrimitiveFunc
  )
  , LispError(
      NumArgs
      , TypeMismatch
      , Parser
      , BadSpecialForm
      , NotFunction
      , UnboundVar
      , Default)
      , Env
      , IOThrowsError
      , liftThrows
      , runIOThrows
      , nullEnv
      , defineVar
      , bindVars
      , setVar
      , getVar
      , ThrowsError
  ) where

import Control.Monad.Error
import Text.ParserCombinators.Parsec
import Data.IORef
import Data.Maybe(isJust)
import System.IO

import Utils

-- LispVal

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle
             | Func { params :: [String]
                    , vararg :: Maybe String
                    , body :: [LispVal]
                    , closure :: Env }

instance Show LispVal where
  show (String contents) = "\"" ++ contents ++ "\""
  show (Atom name)       = name
  show (Number contents) = show contents
  show (Bool True)       = "#t"
  show (Bool False)      = "#f"
  show (List contents)   = "(" ++ unwordsList contents ++ ")"
  show (PrimitiveFunc _) = "<primitive>"
  show (Port _)          = "<IO port>"
  show (IOFunc _)        = "<IO primitive>"
-- show implementation for Dotted list..
  show (DottedList headList tailList) =
    let elem1 = unwordsList headList
        elem2 = show tailList
    in "(" ++ elem1 ++ " . " ++ elem2 ++ ")"
-- show implementation for lambda.
  show (Func {params = args, vararg = varargs, body = _, closure = _ }) =
    let arguments = unwords (map show args)
        vargs = case varargs of
                  Nothing -> ""
                  Just arg -> " . " ++ arg
    in "(lambda (" ++ arguments ++ vargs ++ ") ...)"

-- LispError
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

type ThrowsError = Either LispError

instance Show LispError where
    show (UnboundVar message varname) = message ++ ": " ++ varname
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (NotFunction message func) = message ++ ": " ++ show func
    show (Parser parseErr) = "Parse error at " ++ show parseErr
    show (NumArgs expected found) = "Expected: " ++ show expected ++
      " args: found values " ++ unwordsList found
    show (TypeMismatch expected found) =
      "Invalid type: expected " ++ expected ++ ", found " ++ show found

instance Error LispError where
    noMsg = Default "An error has occured."
    strMsg = Default

trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

type Env = IORef [(String, IORef LispVal)]

type IOThrowsError = ErrorT LispError IO

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = liftM extractValue (runErrorT (trapError action))

isBound :: Env -> String -> IO Bool
isBound envRef var = liftM (isJust . lookup var) (readIORef envRef)

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
          (liftIO . readIORef)
          (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
    env <- liftIO (readIORef envRef)
    maybe (throwError $ UnboundVar "Setting an unbound variable" var)
          (liftIO . (`writeIORef` value))
          (lookup var env)
    return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO (isBound envRef var)
    if alreadyDefined
      then setVar envRef var value >> return value
      else liftIO $ newDefineVar envRef var value

newDefineVar :: Env -> String -> LispVal -> IO LispVal
newDefineVar envRef var value = do
    valueRef <- newIORef value
    env <- readIORef envRef
    writeIORef envRef ((var, valueRef) : env)
    return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings2 env = liftM (++ env) (mapM addBinding bindings2)
        addBinding (var, value) = do
          ref <- newIORef value
          return (var, ref)

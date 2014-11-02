module Variables(
                Env
                , IOThrowsError
                , liftThrows
                , runIOThrows
                , nullEnv
                , defineVar
                , bindVars
                , setVar
                , getVar
                ) where

import Data.IORef
import LispValue
import LispError
import Control.Monad.Error
import Data.Maybe(isJust)

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
  env <- liftIO (readIORef envRef)
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
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)


module Repl (
  runRepl
  , runOne
  ) where

import System.IO
import Control.Monad

import Evaluation
import Lisp

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr =
  runIOThrows $ liftM show $ liftThrows (readExpr expr) >>=
  evaluating env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

-- repeat until predicate is satified.
-- name_ is a convention in haskell for expressing this behavior.
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
  result <- prompt
  unless (predicate result) (action result >> until_ predicate prompt action)

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl =
  primitiveBindings >>=  until_ (== "quit") (readPrompt "Scheme>>> ") .
  evalAndPrint


module Repl (runRepl, evalAndPrint) where

import System.IO
import Control.Monad
import Evaluation

import LispError

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return (extractValue (trapError
                         (liftM show (readExpr expr >>= evaluating))))

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

-- repeat until predicate is satified.
-- name_ is a convention in haskell for expressing this behavior.
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
  result <- prompt
  unless (predicate result) (action result >> until_ predicate prompt action)

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Scheme>>> ") evalAndPrint


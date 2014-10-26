module Main (main) where

import System.Environment
import Evaluation
import LispError
import Control.Monad

-- import qualified Control.Monad as CM

main :: IO ()
main = do
  args <- getArgs
  evaluated <- return (liftM show (readExpr (head args) >>= evaluating))
  putStrLn (extractValue (trapError evaluated))

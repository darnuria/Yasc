module Main (main) where

import System.Environment
import Evaluation
import LispError
import Control.Monad

-- import qualified Control.Monad as CM

main :: IO ()
main = do
  args <- getArgs
  let evaluated = liftM show (readExpr (head args) >>= evaluating)
    in putStrLn (extractValue (trapError evaluated))

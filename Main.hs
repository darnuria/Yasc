module Main (main) where

import System.Environment
--import Control.Monad

--import Evaluation
--import LispError
import Repl

main :: IO ()
main = do
  args <- getArgs
  dispatch args

dispatch :: [String] -> IO ()
dispatch [] = runRepl
dispatch [x] = evalAndPrint x
dispatch _ = putStrLn "Program takes only 0 or 1 argument"

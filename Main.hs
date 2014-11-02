module Main (main) where

import System.Environment

import Repl

main :: IO ()
main = do
  args <- getArgs
  dispatch args

dispatch :: [String] -> IO ()
dispatch [] = runRepl
dispatch [arg] = runOne arg
dispatch _ = putStrLn "Program takes only 0 or 1 argument"

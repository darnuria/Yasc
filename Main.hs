module Main (main) where

import System.Environment
import qualified Text.ParserCombinators.Parsec as P
import Parsing
import Evaluation
import LispError

-- import qualified Control.Monad as CM

main :: IO ()
main = getArgs >>= print . evaluating . readExpr . head

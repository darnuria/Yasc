module Parsing (
  parseNumber,
  parseList,
  parseExpr,
  parseDottedList,
  parseAtom,
  parseQuoted,
  parseString,
  ) where

import qualified Text.ParserCombinators.Parsec as P
import Text.Parsec.Char hiding (spaces)
import LispValue

parseNumber :: P.Parser LispVal
parseNumber = do
  numbers <- P.many1 digit
  let f = Number . read in return (f numbers)

parseList :: P.Parser LispVal
parseList = do
  expr <- P.sepBy parseExpr spaces
  return (List expr)

parseDottedList :: P.Parser LispVal
parseDottedList = do
  headList <- P.endBy parseExpr spaces
  tailList <- char '.' >> spaces >> parseExpr
  return (DottedList headList tailList)

parseQuoted :: P.Parser LispVal
parseQuoted = do
  char '\''
  expr <- parseExpr
  return (List [Atom "quote", expr])

parseAtom :: P.Parser LispVal
parseAtom = do
  first <- letter P.<|> symbol
  rest <- P.many (letter P.<|> digit P.<|> symbol)
  let atom = first:rest in
    return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                _    -> Atom atom

parseString :: P.Parser LispVal
parseString = do
  char '"'
  x <- P.many (noneOf "\"")
  char '"'
  return (String x)

parseExpr :: P.Parser LispVal
parseExpr = parseAtom P.<|>
  parseString P.<|>
  parseNumber P.<|>
  parseQuoted P.<|> do
    char '('
    vals <- P.try parseList P.<|> parseDottedList
    char ')'
    return vals

symbol :: P.Parser Char
symbol = P.oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: P.Parser ()
spaces = P.skipMany1 space


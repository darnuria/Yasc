module LispError (
  ThrowsError,
  trapError
  ) where

import Control.Monad.Error
import Text.ParserCombinators.Parsec
import LispValue

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
                                  " args: found values " ++
                                  unwordsList found
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++
                                       ", found " ++ show found

instance Error LispError where
  noMsg = Default "An error has occured."
  strMsg = Default

trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)

extracValue :: ThrowsError a -> a
extracValue (Right val) = val


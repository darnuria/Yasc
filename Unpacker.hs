{-# LANGUAGE ExistentialQuantification #-}
module Unpacker (
                Unpacker(AnyUnpacker),
                unpackEquals,
                unpackString,
                unpackBool,
                unpackNumber
                ) where

import LispValue
import LispError
import Control.Monad.Error

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return (unpacked1 == unpacked2)
  `catchError` const (return False)

unpackNumber :: LispVal -> ThrowsError Integer
unpackNumber (Number n) = return n
unpackNumber (String n) =
    let parsed = reads n in
      if null parsed
        then throwError (TypeMismatch "number" (String n))
        else return (fst (head parsed))
unpackNumber (List [n]) = unpackNumber n
unpackNumber notNum = throwError (TypeMismatch "number" notNum)

unpackString :: LispVal -> ThrowsError String
unpackString (String str) = return str
unpackString (Number str) = return (show str)
unpackString (Bool str) = return (show str)
unpackString notString = throwError (TypeMismatch "string" notString)

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError (TypeMismatch "Boolean" notBool)


module LispValue (
  LispVal(Atom, List, DottedList, Number, String, Bool),
  unpackNumber,
  unwordsList
  ) where


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where
  show (String contents) = "\"" ++ contents ++ "\""
  show (Atom name) = name
  show (Number contents) = show contents
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (List contents) = "(" ++ unwordsList contents ++ ")"
  show (DottedList headList tailList) = let elem1 = unwordsList headList
                                            elem2 = show tailList
                                        in "(" ++ elem1 ++ " . " ++ elem2 ++ ")"
unpackNumber :: LispVal -> Integer
unpackNumber (Number n) = n
unpackNumber (String n) =
  let parsed = read n :: [(Integer, String)] in
    if null parsed
      then 0
      else fst (head parsed)
unpackNumber (List [n]) = unpackNumber n
unpackNumber _ = 0

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

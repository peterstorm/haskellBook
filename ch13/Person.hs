module Person where

type Name = String
type Age = Integer

data Person = Person Name Age
  deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknow String
                   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == ""            = Left NameEmpty
  | not (age > 0)         = Left AgeTooLow
  | otherwise             = Left $ PersonInvalidUnknow
                                   $ "Name was: " ++ show name ++
                                     " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Please input name: "
  name <- getLine
  putStrLn "Please input age: "
  age <- getLine
  let person = mkPerson name (read age)
  case person of
    Right x -> putStrLn $ "Yay, succesfully got a person: " ++ show x
    Left x  -> putStrLn $ show x

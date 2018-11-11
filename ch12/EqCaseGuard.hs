module EqCaseGuard where

type Name = String
type Age = Integer

type ValidatePerson a = Either [PersonInvalid] a

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Eq, Show)

ageValidate :: Age -> Either [PersonInvalid] Age
ageValidate age = case age >= 0 of
                True  -> Right age
                False -> Left [AgeTooLow]

nameValidate :: Name -> Either [PersonInvalid] Name
nameValidate name = case name /= "" of
                      True  -> Right name
                      False -> Left [NameEmpty]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age =
  mkPerson' (nameValidate name) (ageValidate age)
    where mkPerson' (Right nameOK) (Right ageOK) = Right (Person nameOK ageOK)
          mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
          mkPerson' (Left badName) _             = Left badName
          mkPerson' _              (Left badAge) = Left badAge


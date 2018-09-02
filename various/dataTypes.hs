data Trivial =
  Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True

data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun
    deriving (Ord, Show)

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

data Date =
  Date DayOfWeek Int
    deriving Show

instance Eq Date where
  (==) (Date weekday dayOfMonth)
      (Date weekday' dayOfMonth') =
        weekday == weekday'
         && dayOfMonth == dayOfMonth'

data Identity a =
  Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

data TisAnInteger =
  TisAnInteger Integer

instance Eq TisAnInteger where
  (==) (TisAnInteger number)
      (TisAnInteger number') =
        number == number'

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two numberOne numberTwo)
      (Two numberOne' numberTwo') =
        numberOne == numberOne'
         && numberTwo == numberTwo'

data StringOrInt =
  TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt a') = a == a'
  (==) (TisAString a) (TisAString a') = a == a'
  (==) _ _ = False

data Pair a =
  Pair a a
    deriving Show

instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair a' b') =
        a == a'
         && b == b'

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') =
        a == a'
         && b == b'

data Which a =
  ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) _ _ = False

data EitherOr a b =
  Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye b) (Goodbye b') = b == b'
  (==) _ _ = False

module PatternMatching where

-- small examples
isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False

-- Username and Accountnumber example
newtype Username =
  Username String

newtype AccountNumber =
  AccountNumber Integer

data User =
    UnregisteredUser
  | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser =
  putStrLn "UnregisteredUser"
printUser (RegisteredUser
            (Username name)
            (AccountNumber accNum)) =
              putStrLn $ name ++ " " ++ show accNum

myUser :: Username
myUser = Username "Johanna"

myAcc :: AccountNumber
myAcc = AccountNumber 1010

rUser :: User
rUser = RegisteredUser myUser myAcc



-- Penguins datatype example (yay!)
data WherePenguinsLive
  = Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin =
  Peng WherePenguinsLive
  deriving (Eq, Show)

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereItLives) = whereItLives

humboldt :: Penguin
humboldt = Peng SouthAmerica

gentoo :: Penguin
gentoo = Peng Antarctica

macaroni :: Penguin
macaroni = Peng Antarctica

little :: Penguin
little = Peng Australia

galapagos :: Penguin
galapagos = Peng Galapagos

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _                = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _                 = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p =
  (galapagosPenguin p) || (antarcticPenguin p)

-- TUBLES
-- first example not pattern matching
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

-- Or written with pattern matching
f' :: (a, b) -> (c, d) -> ((b, d), (a, c))
f' (a, b) (c, d) = ((b, d), (a, c))

addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

addEmUp2Alt :: Num a => (a, a) -> a
addEmUp2Alt tup = (fst tup) + (snd tup)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, x) = x

-- EXERCISES
f'' :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f'' (a, b, c) (d, e, f) = ((a, d), (c, f))



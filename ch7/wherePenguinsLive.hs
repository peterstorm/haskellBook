module WherePenguinsLive where

data WherePenguinsLive 
  = Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin
  = Peng WherePenguinsLive
  deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _           = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereItLives) =
  whereItLives

-- let's make some penguis!
humboldt :: Penguin
humboldt = Peng SouthAmerica

gentoo :: Penguin
gentoo = Peng Antarctica

little :: Penguin
little = Peng Australia

galapagos :: Penguin
galapagos = Peng Galapagos

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _                = False

antarticPenguin :: Penguin -> Bool
antarticPenguin (Peng Antarctica) = True
antarticPenguin _                 = False

antarticOrGalapagos :: Penguin -> Bool
antarticOrGalapagos p =
     (galapagosPenguin p)
  || (antarticPenguin p)

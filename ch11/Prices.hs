module Prices where

data Price = Price Integer
  deriving (Eq, Show)
           
data Size = Size Integer
  deriving (Eq, Show)

data Manufacturer = Mini
                  | Mazda
                  | Tata
                  deriving (Eq, Show)

data Airline = PapuAir
             | CatapultsR'Us
             | TakeYourChanceUnited
             deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

-- samples
myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 10)

cars = [myCar, urCar, clownCar]

-- functions
isCar :: Vehicle -> Bool
isCar v = case v of
            (Car _ _) -> True
            _         -> False

isPlane :: Vehicle -> Bool
isPlane p = case p of
              (Plane _ _) -> True
              _         -> False

areCars :: [Vehicle] -> Bool
areCars [] = False
areCars vs = foldr (&&) True (fmap isCar vs)

getManufacturer :: Vehicle -> Manufacturer
getManufacturer v = case v of
                      (Car x _) -> x
                      _         -> error "Not a car"

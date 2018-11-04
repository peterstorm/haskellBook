# Chapter 11 - Algebraic datatypes

### Exercises: Dog Types

1.
    Is `Doggies` a type- or data constructor?  
    a) Type constructor

2.
    What is the kind of `Doggies`?  
    a) `* -> *`

3.
    What is the kind of `Doggies String`?  
    a) `*`

4.
    What is the type of `Husky 10`?  
    a) `Num a => Doggies a`

5.
    What is the type of `Husky (10 :: Integer)?  
    a) `Doggies Integer`

6.
    What is the type of `Mastiff "Scooby Doo"`?  
    a) `Doggies String`

### Exercises: Vehicles

1.
    What is the type of `myCar`?  
    a) `myCar :: Vehicle`

2.
    Define the following functions:
    ```haskell
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
    ```

3.
    Write a function that tells of the manufacturer of a piece of data:
    ```haskell
    getManufacturer :: Vehicle -> Manufacturer
    getManufacturer v = case v of
                          (Car x _) -> x
                          _         -> error "Not a car"
    ```

4.
    The function will throw an error.

5.
    Data types add, and final file:
    ```haskell
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
    ```

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

### newtype

Let say we have a function that tells us if we have too many goats:
```haskell
tooManyGoats :: Int -> Bool
tooManyGoats n = n > 42
```

The problem is we might confuse this function with something that could tell us if we have too many cows or horses, because the argument is just an `Int`.

Bring in `newtype`.

```haskell
newType Goats = Goats Int
  deriving (Eq, Show)

newtype Cows = Cows Int
  deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42
```

Now we can only give the function a type of `Goats`, and never make the mistake of well, mistaking cows for goats.

### Exercises: Logic Goats

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}

module Newtype where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

-- define the Goats instance to behave different than the underlying type Int.
--instance TooMany Goats where
--  tooMany (Goats n) = n > 42

instance TooMany (Int, String) where
  tooMany (n, _) = tooMany n

--instance TooMany (Int, Int) where
--  tooMany (x, y) = tooMany (x + y)

instance (Num a, Ord a) => TooMany (a, a) where
  tooMany (x, y) = (x + y) > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)
```

### Exercises: Pity the Bool

```haskell
module PityTheBool where

import Data.Int

data BigSmall = Big Bool
              | Small Bool
              deriving (Eq, Show)

-- the cardinality of the datatype is:
-- Big Bool = 2
-- Small Bool = 2
-- Total = 4

data NumberOrBool = Numba Int8
                  | BoolyBool Bool
                  deriving (Eq, Show)

-- the cardinality of the datatype is:
-- Numa Int8 = 256
-- BoolyBool Bool = 2
-- Total = 258
```

### Exercises: Programmers
```haskell
module Programmers where

data OperatingSystem = GnuPlusLinux
                     | OpenBSD
                     | Mac
                     | Windows
                     deriving (Eq, Show)

data ProgLang = Haskell
              | Agda
              | Idris
              | PureScript
              deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgLang }
             deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSD
  , Mac
  , Windows
  ]

allLanguages :: [ProgLang]
allLanguages =
  [ Haskell
  , Agda
  , Idris
  , PureScript
  ]

allProgrammers :: [Programmer]
allProgrammers = [Programmer x y | x <- allOperatingSystems, y <- allLanguages]
```

### Binary Tree
```haskell
module BinaryTree where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf                = Leaf
mapTree f (Node left a right) = Node (mapTree f left) 
                                     (f a) 
                                     (mapTree f right)

testTree' :: BinaryTree Integer
testTree'= Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf) 2
  (Node Leaf 3 Leaf)

mapExpected :: BinaryTree Integer
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay :: IO ()
mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then putStrLn "Map, yup okay!"
  else error "test failed"

preorder :: BinaryTree a -> [a]
preorder Leaf                = []
preorder (Node left a right) = [a] ++ (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf                = []
inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf                = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]

testPreorder :: IO () 
testPreorder =
  if preorder testTree == [2, 1, 3] 
  then putStrLn "Preorder fine!" 
  else putStrLn "Bad news bears."

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf                = b
foldTree f b (Node left a right) = f a (foldTree f (foldTree f b left) right)

testInorder :: IO () 
testInorder =
  if inorder testTree == [1, 2, 3] 
  then putStrLn "Inorder fine!" 
  else putStrLn "Bad news bears."

testPostorder :: IO () 
testPostorder =
  if postorder testTree == [1, 3, 2] 
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

testFoldTree :: IO ()
testFoldTree =
  if foldTree (+) 0 testTree == 6
  then putStrLn "foldMap fine!"
  else putStrLn "foldMap failed check"

main :: IO ()
main = do
  mapOkay
  testPreorder
  testInorder
  testPostorder
  testFoldTree
```

### Vigenere

```haskell
module Vigenere where

import Data.Char

createKey :: String -> String -> String
createKey k m = go k m ""
  where go (y : ys) (x : xs) acc
          | x == ' '           = go (y : ys) xs (' ' : acc)
          | otherwise          = go ys xs (y : acc)
        go "" (x : xs) acc     = go k (x : xs) acc
        go _ "" acc            = reverse acc

keyToNumeric :: String -> [Int]
keyToNumeric k = go k []
  where go (x : xs) acc
          | isLower x = go xs (ord x - ord 'a' : acc)
          | otherwise = go xs (ord x - ord 'A' : acc)
        go [] acc = reverse acc

shiftRightN :: Int -> Char -> Char
shiftRightN n c
  | c == ' '    = ' '
  | isLower c   = chr $ (+ ord 'a') $ mod (n + (ord c - ord 'a')) 26
  | isUpper c   = chr $ (+ ord 'A') $ mod (n + (ord c - ord 'A')) 26

ceasar :: Int -> String -> String
ceasar i m = fmap (shiftRightN i) m

unceasar :: Int -> String -> String
unceasar i m = fmap (shiftRightN (negate i)) m

vigenere :: String -> String -> String
vigenere k m = zipWith shiftRightN (keyToNumeric $ createKey k m) m

unvigenere :: String -> String -> String
unvigenere k m = zipWith shiftRightN (fmap negate $ keyToNumeric $ createKey k m) m
```

### As-pattersn and Language exercises
```haskell
module Test where

import Data.Char

doubleUp :: [a] -> [a]
doubleUp xs@(x : _) = (x : xs)

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _                     = True
isSubseqOf _ []                     = False
isSubseqOf (x : xs) word@(y : ys) = if x `elem` word
                                         then isSubseqOf xs ys
                                         else False

capitalizeWords :: String -> [(String, String)]
capitalizeWords "" = []
capitalizeWords phrase = fmap toTuple $ words phrase
  where toTuple s@(y : ys) = (s , (toUpper y : ys))

capitalizeWord :: String -> String
capitalizeWord ""       = ""
capitalizeWord (x : xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph xs = unwords . fmap capitalizeWord $ words xs
```

### Phone exercise.
I'm not super satisfied with the efficiency of the implementation, because I traverse _alot_ of lists, _alot_ of times :D
But it was the first thing that came to mind. I should have probably used more folds with comparison functions.

```haskell
module Phone where

import Data.List
import Data.Char

data Phone   = Phone [Button]

data Button  = Button Digit String
             deriving Show

type Digit   = Char
type Presses = Int

phone :: Phone
phone = Phone
    [ Button '1' "1"
    , Button '2' "abc2"
    , Button '3' "def3"
    , Button '4' "ghi4"
    , Button '5' "jkl5"
    , Button '6' "mno6"
    , Button '7' "pqrs7"
    , Button '8' "tuv8"
    , Button '9' "wxyz9"
    , Button '*' "^"
    , Button '0' " 0"
    , Button '#' ".," ]

convo :: [String]
convo =
    [ "Wanna play 20 questions"
    , "Ya"
    , "U 1st haha"
    , "Lol ok. Have u ever tasted alchohol"
    , "Lol ya"
    , "Wow ur cool haha. Ur turn"
    , "Ok. Do u think I am pretty Lol"
    , "Lol ya"
    , "Just making sure rofl ur turn" ]



getButton :: Phone -> Char -> [Button]
getButton (Phone b) c = go (Phone b) c []
  where go p@(Phone b) c acc
          | isUpper c = go p (toLower c) (Button '*' "^" : acc)
          | otherwise = acc ++ filter isDigit b
            where isDigit (Button d s) = c `elem` s

reverseTaps :: Phone -> Char -> [(Digit, Presses)]
reverseTaps p c = fmap toTuple $ getButton p c
  where toTuple (Button d s) = (d, (+1) $ foldr (+) 0 $ elemIndices (toLower c) s)

cellPhonesDead :: Phone -> String -> [(Digit, Presses)]
cellPhonesDead p s = concat $ fmap (reverseTaps p) s

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps xs = foldr ((+) . snd) 0 xs

countCharacter :: String -> Char -> (Char, Int)
countCharacter str c = toTuple $ filter (==c) str
  where toTuple str@(x : xs) = (x, length str)

countCharacters :: String -> [(Char, Int)]
countCharacters str = fmap (countCharacter str) str

findLetter :: [(Char, Int)] -> (Char, Int)
findLetter = foldr (\a b -> if (snd a) >= (snd b) then a else b) ('0', 0)

filterSpaces :: [(Char, Int)] -> [(Char, Int)]
filterSpaces = filter (\a -> (fst a) /= ' ')

mostPopularLetter :: String -> Char
mostPopularLetter = fst . findLetter . filterSpaces . countCharacters

costOfMostPopularLetter :: String -> Phone -> Presses
costOfMostPopularLetter str p = fingerTaps . cellPhonesDead p $ createString . findLetter . filterSpaces $ countCharacters str
  where createString (a, b) = take b $ repeat a

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

filterDotsAndCommas :: [String] -> [String]
filterDotsAndCommas xs = fmap (takeWhile (/=',') . takeWhile (/='.')) xs

countWord :: [String] -> String -> (String, Int)
countWord xs str = toTuple $ filter (==str) xs
  where toTuple str@(x : xs) = (x, length str)

countWords :: [String] -> [(String, Int)]
countWords xs = fmap (countWord xs) xs

findWord :: [(String, Int)] -> (String, Int)
findWord = foldr (\a b -> if (snd a) >= (snd b) then a else b) ("", 0)

coolestWord :: [String] -> String
coolestWord xs = fst . findWord . countWords . filterDotsAndCommas . concat $ fmap words xs
```

### Huttons's Razor

```haskell
eval :: Expr -> Integer
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add e1 e2) = (printExpr e1) ++ " + " ++ (printExpr e2)
```

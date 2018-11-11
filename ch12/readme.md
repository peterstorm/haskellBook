# Chapter 12 - Signaling Adversity

### EQ Case Guard
```haskell
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
```

### String processing
```haskell
module Strings where

import Data.Char

notThe :: String -> Maybe String
notThe str
  | str == "the" = Nothing
  | otherwise    = Just str

notThe' :: String -> Maybe String
notThe' ('t':'h':'e':' ':xs) = Nothing
notThe' xs                   = Just xs

-- non recursive I guess
replaceThe :: String -> String
replaceThe str = unwords . fmap addAnA $ fmap notThe $ words str
  where addAnA x = case x of
                     Nothing -> "a"
                     Just x  -> x

-- recursive
replaceThe' :: String -> String
replaceThe' ""  = ""
replaceThe' str = case notThe' str of
                    Nothing       -> 'a' : replaceThe' (drop 3 str)
                    Just (y : ys) -> y   : replaceThe' ys

isVowel :: String -> Bool
isVowel (x : xs) = x `elem` "aeuio"



countTheBeforeVowel :: String -> Integer
countTheBeforeVowel xs = go xs 0
  where go str@(x : xs) acc
          | notThe' str == Nothing, isVowel (take 1 $ drop 4 str) = go (drop 4 str) (acc + 1)
          | otherwise                                             = go (dropWhile isSpace $ dropWhile (isAlpha) xs) acc
        go "" acc = acc

countVowels :: String -> Integer
countVowels xs = go xs 0
  where go str@(x : xs) acc
          | isVowel str = go xs (acc + 1)
          | otherwise   = go xs acc
        go "" acc = acc
```

### Validate the word
```haskell
module ValidateWord where

newtype Word' =
  Word' String
  deriving (Eq, Show)

isVowel :: Char -> Bool
isVowel x = x `elem` "aeuio"

mkWord :: String -> Maybe Word'
mkWord xs = if (length $ filter (not . isVowel) xs) > (length $ filter isVowel xs)
               then Just $ Word' xs 
               else Nothing
```

### It's only Natural
```haskell
module Natural where

data Nat = Zero
         | Succ Nat
         deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0     = Nothing
  | i == 0    = Just Zero
  | otherwise = Just (f i)
    where f 0 = Zero
          f n = Succ $ f (n - 1)
```

### Small library for Maybe
```haskell
module LibMaybe where

isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just _) = False

mayyBee :: b -> (a -> b) -> Maybe a -> b
mayyBee b _ Nothing  = b
mayyBee b f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing  = a
fromMaybe _ (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe []      = Nothing
listToMaybe (x : _) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes = fmap cata . filter isJust
  where cata (Just x) = x

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe xs = if and $ fmap isJust xs 
                  then Just (catMaybes xs)
                  else Nothing
```

### Small library for Either
```haskell
module LibEither where

lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where f (Left a) b = a : b
        f _        b = b

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where f (Right b) a = b : a
        f _         a = a

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b)  = Just $ f b

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)  = f a
either' _ g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\a -> Nothing) (\b -> Just $ f b)
```

### Unfolds
```haskell
module Unfolds where

myIterate :: (a -> a) -> a -> [a]
myIterate f a = [a] ++ (myIterate f (f a))

myUnfoldr :: (b -> Maybe (a, b)) ->  b -> [a]
myUnfoldr f b = case f b of
                  Nothing     -> []
                  Just (a, b) -> a : myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just (b, f b)) x
```

### Treeeees
```haskell
module BinaryTree where

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case f a of
               Nothing        -> Leaf
               Just (x, y, z) -> Node (unfold f x) y (unfold f z)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
  where f a
          | a < n     = Just (a + 1, a, a + 1)
          | otherwise = Nothing
```

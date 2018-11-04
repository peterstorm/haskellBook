### Exercises: Understanding Folds

1.
   `foldr (*) 1 [1..5]` will return the same result as which of the following:  
   Answer: b) `foldl (flip (*)) 1 [1..5]` and c) `foldl (*) 1 [1..5]`

2.
   Write out the evaluation steps for `foldl (flip (*)) 1 [1..3]`:
   ```haskell
   foldl (flip (*)) 1 [1..3]
   (*) 1 1
   (*) 2 ((*) 1 1)
   (*) 3 ((*) 2 ((*) 1 1)
   (*) 3 ((*) 2 1)
   (*) 3 2
   6
   ```

3.
   One difference between `foldr` and `foldl` is:  
   c) `foldr`, but not `foldl`, associates to the right.

4.
   Folds are catamorphisms, which means they are generally used to:  
   a) reduce structure

5.
   Fix the functions, the answers are given below:
   ```haskell
   -- a)
   foldr (++) [] ["woot", "Woot"]

   -- b)
   foldr max '\NUL' "fear is the little death"

   -- c)
   foldr (&&) True [False, True]

   -- d)
   foldr (||) False [False, True]

   -- e)
   foldl (++) "" (map show [1..5])

   -- f)
   foldr const 0 [1..5]

   -- g)
   foldr const '\NUL\ "tacos"

   -- h)
   foldl (flip const) '\NUL' "burritos"

   -- i)
   foldl (flip const) 0 [1..5]
   ```

### How to write fold functions

DbExercise.hs

```haskell
module DbExercise where

import Data.Time

data DatabaseItem = DbString String
                 | DbNumber Integer
                 | DbDate UTCTime
                 deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 0001
  , DbString "Hello, World!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  , DbDate (UTCTime (fromGregorian 1921 6 1) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate db = [x | DbDate x <- db ]

filterDbDate' :: [DatabaseItem] -> [UTCTime]
filterDbDate' db = foldr f [] db
  where f (DbDate a) b = a : b
        f _ b          = b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber db = [x | DbNumber x <- db]

filterDbNumber' :: [DatabaseItem] -> [Integer]
filterDbNumber' [] = []
filterDbNumber' db = foldr f [] db
  where f (DbNumber a) b = a : b
        f _ b            = b

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = f . filterDbDate
  where f (x : xs) = foldr (\a b -> if a > b then a else b) x xs

mostRecent' :: [DatabaseItem] -> UTCTime
mostRecent' = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = f . filterDbNumber
  where f xs = foldr (+) 0 xs

sumDb' :: [DatabaseItem] -> Integer
sumDb' = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb = (/2) . fromIntegral . f . filterDbNumber
  where f xs = foldr (+) 0 xs

avgDb' :: [DatabaseItem] -> Double
avgDb' = (/2) . fromIntegral . sum . filterDbNumber
```

### Scan Exercises

1.
   Modify your `fibs` function to only return the first 20 Fibonacci numbers:
   ```haskell
   fibs20 :: [Integer]
   fibs20 = take 20 f
     where f = 1 : scanl (+) 1 fibs
   ```

2.
   Modify `fibs` to return the Fibonacci numbers that are less than 100:
   ```haskell
   fibs100 :: [Integer]
   fibs100 = [x | x <- f, x < 100]
     where f = 1 : scanl (+) 1 f
   ```

3.
    Rewrite `factorial` using `scanl`:
    ```haskell
    factorialScan :: [Integer]
    factorialScan = scanl (*) 1 [1..]

    factorialScanN :: Int -> [Integer]
    factorialScanN n = take n factorialScan
    ```

### Chapter Exercises

1.
    Given the following sets of consonants and vowels:
    ```
    stops = "pbtdkg"
    vowels = "aeiou"
    ```
    a) Write a function that takes inputs from `stops` and `vowels` and make 3-tuples of all possible stop-vowel-stop
    combinations.  
    ```haskell
    threeLetters :: [(Char, Char, Char)]
    threeLetters = [(x, y, z) | x <- stops, y <- vowels, z <- stops]
    ```  
    b) Modify that function so that it only returns the combinations that begin with a p.  
    ```haskell
    threeLettersOnlyP :: [(Char, Char, Char)]
    threeLettersOnlyP = [(x, y, z) | x <- "p", y <- vowels, z <- stops]
    ```  
    c) Now write a function that takes nouns and verbs, and do a 3 tuple of noun-verb-noun:  
    ```haskell
    returnSentences :: [String] -> [String] -> [(String, String, String)]
    returnSentences nouns verbs = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns ]
    ```



### Rewriting functions using folds:

```haskell
myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (||) False (fmap f xs)

myElem :: Eq a => a -> [a] -> Bool
myElem x xs = foldr (||) False (fmap (==x) xs)

myElem' :: (Eq a, Foldable t) => a -> t a -> Bool
myElem' x xs = any (==x) xs

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f xs = foldr (\x b -> f x : b) [] xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x : xs) = foldr (\x b -> if (f x) then (x : b) else b) [] (x : xs)

squish :: [[a]] -> [a]
squish = foldr const []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = foldr (\x b -> f x ++ b) [] xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x : xs) = foldl (\x y -> case f x y of GT -> x; _ -> y) x (x : xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x : xs) = foldl (\x y -> case f x y of LT -> x; _ -> y) x (x : xs)
```

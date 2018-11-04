module ChapterExercises where

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

noun :: [String]
noun = [ "apple"
       , "car"
       , "man"
       ]

verb :: [String]
verb = [ "walks"
       , "falls"
       , "yells"
       ]

threeLetters :: [(Char, Char, Char)]
threeLetters = [(x, y, z) | x <- stops, y <- vowels, z <- stops]

threeLettersOnlyP :: [(Char, Char, Char)]
threeLettersOnlyP = [(x, y, z) | x <- "p", y <- vowels, z <- stops]

returnSentences :: [String] -> [String] -> [(String, String, String)]
returnSentences nouns verbs = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns ]

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

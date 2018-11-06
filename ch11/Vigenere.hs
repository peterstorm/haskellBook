module Vigenere where

import Data.Char

-- i use this to create the pattern formed from the KEY=k and the MESSAGE=m you supply
-- it returns the pattern ALLY AL LYAL for ALLY=k and MEET ME AT DAWN=m
createKey :: String -> String -> String
createKey k m = go k m ""
  where go (y : ys) (x : xs) acc
          | x == ' '           = go (y : ys) xs (' ' : acc)
          | otherwise          = go ys xs (y : acc)
        go "" (x : xs) acc     = go k (x : xs) acc
        go _ "" acc            = reverse acc

-- because I later want to use zipWith I transform the key from createKey to a list of Int
keyToNumeric :: String -> [Int]
keyToNumeric k = go k []
  where go (x : xs) acc
          | isLower x = go xs (ord x - ord 'a' : acc)
          | otherwise = go xs (ord x - ord 'A' : acc)
        go [] acc = reverse acc

-- this is the function I use to shift the letters, it takes the NUMBER=i of spaces to shift with and a CHARACTER=c
-- it returns the shifted character
shiftRightN :: Int -> Char -> Char
shiftRightN n c
  | c == ' '    = ' '
  | isLower c   = chr $ (+ ord 'a') $ mod (n + (ord c - ord 'a')) 26
  | isUpper c   = chr $ (+ ord 'A') $ mod (n + (ord c - ord 'A')) 26

-- takes a NUMBER=i to switch with, and a MESSAGE=m to shift, returns the shifted message
ceasar :: Int -> String -> String
ceasar i m = fmap (shiftRightN i) m

-- unwinds the ceasar encrypted word
unceasar :: Int -> String -> String
unceasar i m = fmap (shiftRightN (negate i)) m

-- here we use zipWith that takes a function and two lists and return a list where each element of the lists are smashed
-- together to one. we use createKey and keyToNumeric to transform the created key into the list of ints we need.
vigenere :: String -> String -> String
vigenere k m = zipWith shiftRightN (keyToNumeric $ createKey k m) m

-- unwinds the vigenere encryption
unvigenere :: String -> String -> String
unvigenere k m = zipWith shiftRightN (fmap negate $ keyToNumeric $ createKey k m) m

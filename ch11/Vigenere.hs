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

module Char where

import Data.Char

filterUpper :: [Char] -> [Char]
filterUpper = filter isUpper

capFirst :: [Char] -> [Char]
capFirst (x : xs) = toUpper x : xs

capAll :: [Char] -> [Char]
capAll []       = []
capAll (x : xs) = toUpper x : (capAll xs)

capOnlyFirst :: [Char] -> Char
capOnlyFirst = toUpper . head


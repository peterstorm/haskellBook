module Cipher where

import Data.Char

upperOrLower :: Char -> Int
upperOrLower x = case isUpper x of
                   False -> (mod ((ord x) - 96) 26) + 96
                   True -> (mod ((ord x) - 64) 26) + 64

encryptRight :: Int -> [Char] -> [Char]
encryptRight _ []       = []
encryptRight i (x : xs) = (chr . (+i) $ upperOrLower x) : (encryptRight i xs)

decryptRight :: Int -> [Char] -> [Char]
decryptRight _ []       = []
decryptRight i (x : xs)
  | isLower x, ((upperOrLower x) - 96 - i) <= 0 = (chr $ 122 - ((upperOrLower x) - 96 - i)) : (decryptRight i xs)
  | isUpper x, ((upperOrLower x) - 64 - i) <= 0 = (chr $ 90 - ((upperOrLower x) - 64 - i)) : (decryptRight i xs)
  | otherwise                                  = (chr $ ((upperOrLower x) - i)) : (decryptRight i xs)


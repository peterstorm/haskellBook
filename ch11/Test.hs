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


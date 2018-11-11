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

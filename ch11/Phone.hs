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

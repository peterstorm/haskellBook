# Chapter 13 - Building Projects

## Hangman
```haskell
module Main where

import Control.Monad (forever)
import Data.Char     (toLower)
import Data.Maybe    (isJust)
import Data.List     (intersperse)
import System.Exit   (exitSuccess)
import System.Random (randomRIO)

data Puzzle =
  Puzzle String [Maybe Char] [Char]

newtype WordList = WordList [String]
  deriving (Eq, Show)

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
      ++ " Guessed so far: " ++ guessed

main :: IO ()
main = do
  word <- getLine
  let puzzle = freshPuzzle $ fmap toLower word
  runGame puzzle


minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList $ filter gameLength aw
    where gameLength w =
            let l = length (w :: String)
             in l >= minWordLength
             && l <  maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, (length wl) - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

freshPuzzle :: String -> Puzzle
freshPuzzle str = Puzzle str (fmap (f) str) []
  where f x = if x == ' ' then Just ' ' else Nothing

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar x = case x of
                       Nothing -> '_'
                       Just x  -> x

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) c = elem c s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ s) c = elem c s

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = Puzzle word newFilledInSoFar (c : s)
  where zipper guess wordChar filledChar = if wordChar == guess
                                             then Just wordChar
                                             else filledChar
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "Correct! This character is in the word!"
      return $ fillInCharacter puzzle guess
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return $ fillInCharacter puzzle guess

gameOver :: Puzzle -> IO ()
gameOver (Puzzle word _ guessed) =
  if (length guessed) > 20
     then do putStrLn "You lose!"
             putStrLn $ "The word was: " ++ word
             exitSuccess
     else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar
     then do putStrLn "You win! Congratulations!"
             exitSuccess
     else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStrLn "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character"
```

### Palindrome
```haskell
module Palindrome where

import Control.Monad
import System.Exit
import Data.Char

palindrome :: IO ()
palindrome = forever $ do
  line <- getLine
  let line1 = concat . words $ fmap toLower $ filter isAlpha line
  case (line1 == reverse line1) of
    True  -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope"
      exitSuccess
```

### Person
```haskell
module Person where

type Name = String
type Age = Integer

data Person = Person Name Age
  deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknow String
                   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == ""            = Left NameEmpty
  | not (age > 0)         = Left AgeTooLow
  | otherwise             = Left $ PersonInvalidUnknow
                                   $ "Name was: " ++ show name ++
                                     " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Please input name: "
  name <- getLine
  putStrLn "Please input age: "
  age <- getLine
  let person = mkPerson name (read age)
  case person of
    Right x -> putStrLn $ "Yay, succesfully got a person: " ++ show x
    Left x  -> putStrLn $ show x
```

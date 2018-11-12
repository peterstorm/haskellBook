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

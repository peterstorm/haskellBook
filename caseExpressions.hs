module CaseExpressions where

-- Pattern matching on result of input
funcZ :: Integer -> String
funcZ x =
  case x + 1 == 1 of
    True -> "AWESOME"
    False -> "wut"

pal :: String -> String
pal xs =
  case xs == reverse xs of
    True -> "yes"
    False -> "no"

pal' :: String -> String
pal' xs =
  case y of
    True -> "yes"
    False -> "no"
  where
      y = reverse xs == xs

greetIfCool :: String -> IO ()
greetIfCool coolness =
  case cool of
    True -> putStrLn "Heeey, what's shakin'!"
    False -> putStrLn "Meh"
  where cool = coolness == "Downright frosty yo"

-- EXERCISES!
functionC :: (Ord a) => a -> a -> a
functionC x y =
  case x > y of
    True -> x
    False -> y

ifEvenAdd2 :: Integer -> Integer
ifEvenAdd2 n =
  case even n of
    True -> n + 2
    False -> n

nums :: (Ord a, Num a) => a -> a
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

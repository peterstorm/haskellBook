module Lists where

safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []       = Nothing
safeTail (x : []) = Nothing
safeTail (_ : xs) = Just xs

{-
EXERCISE:
Write your own Enum functions for the provided types
-}

eftBool :: Bool -> Bool -> [Bool]
eftBool True False  = []
eftBool True True   = True : []
eftBool False False = False : []
eftBool False True = False : True : []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd GT GT      = GT : []
eftOrd GT _       = []
eftOrd start stop = go start stop []
  where go start stop list
          | start == stop = list ++ [start]
          | otherwise     = go (succ start) stop (list ++ [start])

eftInt :: Int -> Int -> [Int]
eftInt start stop = go start stop []
  where go start stop list
          | start > stop  = []
          | start == stop = list ++ [start]
          | otherwise     = go (succ start) stop (list ++ [start])

eftChar :: Char -> Char -> [Char]
eftChar start stop = go start stop []
  where go start stop list
          | start > stop   = []
          | start == stop = list ++ [start]
          | otherwise      = go (succ start) stop (list ++ [start])

{-
EXERCISE: Thy Fearful Symmetry 1
Using `takeWhile` and `dropWhile`, write a function that takes a string and returns
a list of strings, using spaces to separate the elements
-}

myWords :: String -> [String]
myWords str = go str []
  where go str list
          | length str == 0 = list
          | otherwise       = go (dropWhile (==' ') (dropWhile (/=' ') str)) (list ++ [takeWhile (/= ' ') str])

{-
EXERCISE:Thy Fearful Symmetry 2
-}

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
\ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines str = go str []
  where go str list
          | length str == 0 = list
          | otherwise       = go
                                (dropWhile (==' ') $ dropWhile (=='\n') $ dropWhile (/='\n') str)
                                (list ++ [takeWhile (/='\n') str])

shouldEqual :: [String]
shouldEqual = ["Tyger Tyger, burning bright", "In the forests of the night", "What immortal hand or eye", "Could frame thy fearful symmetry?"]


main :: IO ()
main =
  print $
    "Are they equal? "
    ++ show (myLines sentences
             == shouldEqual)

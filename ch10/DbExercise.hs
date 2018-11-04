module DbExercise where

import Data.Time

data DatabaseItem = DbString String
                 | DbNumber Integer
                 | DbDate UTCTime
                 deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 0001
  , DbString "Hello, World!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  , DbDate (UTCTime (fromGregorian 1921 6 1) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate db = [x | DbDate x <- db ]

filterDbDate' :: [DatabaseItem] -> [UTCTime]
filterDbDate' db = foldr f [] db
  where f (DbDate a) b = a : b
        f _ b          = b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber db = [x | DbNumber x <- db]

filterDbNumber' :: [DatabaseItem] -> [Integer]
filterDbNumber' [] = []
filterDbNumber' db = foldr f [] db
  where f (DbNumber a) b = a : b
        f _ b            = b

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = f . filterDbDate
  where f (x : xs) = foldr (\a b -> if a > b then a else b) x xs

mostRecent' :: [DatabaseItem] -> UTCTime
mostRecent' = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = f . filterDbNumber
  where f xs = foldr (+) 0 xs

sumDb' :: [DatabaseItem] -> Integer
sumDb' = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb = (/2) . fromIntegral . f . filterDbNumber
  where f xs = foldr (+) 0 xs

avgDb' :: [DatabaseItem] -> Double
avgDb' = (/2) . fromIntegral . sum . filterDbNumber

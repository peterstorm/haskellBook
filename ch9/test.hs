module Test where

safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []       = Nothing
safeTail (x : []) = Nothing
safeTail (x : xs) = Just xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []       = False
myAny f (x : xs) = (f x) || (myAny f xs)

myElem :: Eq a => a -> [a] -> Bool
myElem _ []       = False
myElem e (x : xs) = (e == x) || (myElem e xs)

myElem' :: (Eq a, Foldable t) => a -> t a -> Bool
myElem' e t = any (e==) t

myReverse :: [a] -> [a]
myReverse list = go list []
  where go [] acc = acc
        go (x : xs) acc = go xs (x : acc)

squish :: [[a]] -> [a]
squish [] = []
squish (x : xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []       = []
squishMap f (x : xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain [] = []
squishAgain xs = squishMap id xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x : xs) = go f xs x
  where go f (x : xs) z =
          case f x z of
            GT -> go f xs x
            LT -> go f xs z
            EQ -> go f xs x
        go _ [] z      = z

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x : xs) = go f xs x
  where go f (x : xs) z =
          case f x z of
            GT -> go f xs z
            LT -> go f xs x
            EQ -> go f xs x
        go _ [] z      = z

myMaximum :: (Ord a) => [a] -> a
myMaximum xs = myMaximumBy compare xs

myMinimum :: (Ord a) => [a] -> a
myMinimum xs = myMinimumBy compare xs

main :: IO ()
main = do
  let head = safeHead [1, 2, 3]
  putStrLn $ "The result of safeHead [1, 2, 3] is: " ++ show head

  let tail = safeTail [1, 2, 3, 4]
  putStrLn $ "The result of safeTail [1, 2, 3, 4] is: " ++ show tail


module Test where

safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []       = Nothing
safeTail (x : []) = Nothing
safeTail (x : xs) = Just xs

eftBool :: Bool -> Bool -> [Bool]
eftBool x y
  | x > y     = []
  | x == y    = [x]
  | otherwise = [False, True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y = go x y []
  where go x y acc
          | x > y  = []
          | x == y =  reverse (x : acc)
          | otherwise = go (succ x) y (x : acc)

eftInt :: Int -> Int -> [Int]
eftInt x y = go x y []
  where go x y acc
          | x > y = []
          | x == y = reverse (x : acc)
          | otherwise = go (succ x) y (x : acc)

eftChar :: Char -> Char -> [Char]
eftChar x y = go x y []
  where go x y acc
          | x > y = []
          | x == y = reverse (x : acc)
          | otherwise = go (succ x) y (x : acc)


main :: IO ()
main = do
  let head = safeHead [1, 2, 3]
  putStrLn $ "The result of safeHead [1, 2, 3] is: " ++ show head

  let tail = safeTail [1, 2, 3, 4]
  putStrLn $ "The result of safeTail [1, 2, 3, 4] is: " ++ show tail

  let eB = eftBool False True
  putStrLn $ "The result of eftBool False True is: " ++ show eB

  let eO = eftOrd LT GT
  putStrLn $ "The result of eftOrd LT GT is: " ++ show eO

  let eI = eftInt 10 20
  putStrLn $ "The result of eftInt 10 12 is: " ++ show eI
  
  let eC = eftChar 'a' 'f'
  putStrLn $ "The result of eftChar 'a' 'f' is: " ++ show eC

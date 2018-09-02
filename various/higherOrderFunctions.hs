module HigherOrderFunctions where

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f = \x y -> f y x

returnLast :: a -> b -> c -> d -> d
returnLast _ _ _ d = d

returnLast' :: a -> (b -> (c -> (d -> d)))
returnLast' _ _ _ d = d

-- Company domain modeling
data Employee
  = Coder
  | Manager
  | Veep
  | CEO
  deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ show e ++
             " is the boss of " ++
             show e'

-- has `compare` buildt in
employeeRank :: Employee -> Employee -> IO ()
employeeRank e e' =
  case compare e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> (flip reportBoss) e e'

-- now takes a function `f` as an argument, used for comparing
employeeRank' :: (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO ()
employeeRank' f e e' =
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> (flip reportBoss) e e'

-- function that abuses the patter matching cases of
-- employeeRank :D
codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _     = GT
codersRuleCEOsDrool _ Coder     = LT
codersRuleCEOsDrool e e' = compare e e'

-- EXERSICES

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

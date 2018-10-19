module Coders where

data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Show, Eq, Ord)

-- lets build a function that should really not be used on it's own, beacuse I can say that a Coder is a boss of a Veep for example, but we need it.
reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ show e ++ " is the boss of " ++ show e'

-- now lets build the function that needs the before created function.
employeeRank :: Employee -> Employee -> IO ()
employeeRank e e' = case compare e e' of
                      GT -> reportBoss e e'
                      EQ -> putStrLn "These employees have the same rank"
                      LT -> (flip reportBoss) e e'

-- now the coders are obviously the backbone of a company and they have access to the code!
changedEmployeeRank :: (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO ()
changedEmployeeRank f e e' = case f e e' of
                               GT -> reportBoss e e'
                               EQ -> putStrLn "These employees have the same rank"
                               LT -> (flip reportBoss) e e'

-- and now that we have a changedEmployeeRank that accepts another function as it's argument, we have a HOF, and we can exploit it!
codersRule :: Employee -> Employee -> Ordering
codersRule Coder Coder = EQ
codersRule Coder _     = GT
codersRule _     Coder = LT
codersRule e e'        = compare e e'

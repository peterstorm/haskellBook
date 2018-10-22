module Test where

safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []       = Nothing
safeTail (x : []) = Nothing
safeTail (x : xs) = Just xs

main :: IO ()
main = do
  let head = safeHead [1, 2, 3]
  putStrLn $ "The result of safeHead [1, 2, 3] is: " ++ show head

  let tail = safeTail [1, 2, 3, 4]
  putStrLn $ "The result of safeTail [1, 2, 3, 4] is: " ++ show tail

module Print3Fixed where

greeting :: String
greeting = "Yaaaarrrr"

printSecond :: IO ()
printSecond = do
  putStrLn greeting

main :: IO ()
main = do
  putStrLn greeting
  printSecond

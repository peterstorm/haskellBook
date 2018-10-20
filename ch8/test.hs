module Test where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

main :: IO ()
main = do
  putStrLn "This is the result of factorial 4:"
  print $ factorial 4

module GreetIfCool2 where

greetIfCool2 :: String -> IO ()
greetIfCool2 coolness =
  if cool coolness
    then putStrLn "eeyyy, what's shakin'?"
  else
    putStrLn "pssssh."
  where cool x = x == "downright frosty yo"


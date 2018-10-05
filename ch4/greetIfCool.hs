module GreetIfCool where

greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool
    then putStrLn "eyyy, what's shakin'?"
  else
    putStrLn "pssshhhhh."
  where cool = coolness == "downright frosty yo"

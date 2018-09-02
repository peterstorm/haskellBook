module GreetIfCool2 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool coolness
     then putStrLn "eyyyyyy! What's shakin'?"
  else
    putStrLn "pssssssh."
  where cool v =
          v == "downright frosty yo"

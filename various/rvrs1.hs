module Reverse where

rvrs :: String -> String
rvrs x = y ++ " " ++ z ++ " " ++  w
  where y = drop 9 x
        z = drop 6 (take 8 x)
        w = take 5 x

main :: IO ()
main = print $ rvrs "Curry is awesome"

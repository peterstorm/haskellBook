module Arith3broken where

main :: IO ()
-- Main = do
-- capitalised function name. Capitalisation is reserver for types and
-- typeclasses
main = do
  -- print 1 + 2
  -- we need parens to group the expression, otherwise we're attempting to add 2
  -- to a printed statement
  print (1 + 2)

  -- putStrLn :: String -> IO ()
  -- so we need to use print
  print 10
  --
  -- print (negate -1)
  -- need parens around -1
  print (negate (-1))

  print ((+) 0 blah)
    where blah = negate 1

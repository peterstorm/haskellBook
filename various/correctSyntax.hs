module CorrectSyntax where

x = (+)

f xs = w `x` 1
  where w = length xs

g = \x -> x

-- does the same as the function below
h = \(x:xs) -> x

i = \ xs -> xs !! 0

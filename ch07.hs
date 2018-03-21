myNum :: Integer
myNum = 1

myVal :: Integer -> Integer
myVal f = f + myNum

bindExp :: Integer -> String
bindExp x =
  let y = 5 in
    "the integer was: " ++ show x
    ++ " and y was: " ++ show y

addOneIfOdd :: Integer -> Integer
addOneIfOdd n = case odd n of
                  True -> f n
                  False -> n
                  where f = \n -> n + 1

addFive :: Integer -> Integer -> Integer
addFive = \x -> \y -> (if x > y then y else x) + 5

mFlip f x y = f y x


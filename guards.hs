module Guards where

-- if-then-else implementation of myAbs
myAbs :: Integer -> Integer
myAbs x =
  if x >= 0
     then x
  else negate x

-- 'guards' implementation of myAbs
myAbs' :: Integer -> Integer
myAbs' x
  | x < 0     = (-x)
  | otherwise = x

-- implemented with `negate`
myAbs'' :: Integer -> Integer
myAbs'' x
  | x < 0     = negate x
  | otherwise = x

bloodNa :: Integer -> String
bloodNa x
  | x < 135     = "too low"
  | x > 145     = "too high"
  | otherwise   = "just right"

isRightTriangle :: (Num a, Eq a)
                => a -> a -> a -> String
isRightTriangle a b c
  | a^2 + b^2 == c^2 = "RIGHT ON"
  | otherwise        = "not a right triangle"

dogYrs :: Integer -> Integer
dogYrs x
  | x <= 0    = 0
  | x <= 1    = x * 15
  | x <= 2    = x * 12
  | x <= 4    = x * 8
  | otherwise = x * 6

avgGrade :: (Fractional a, Ord a)
         => a -> Char
avgGrade x
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  | y <  0.59 = 'F'
  where y = x / 100

-- another implementation of the `palindrome` function
-- using guards
pal :: Eq a => [a] -> Bool
pal xs
  | xs == reverse xs = True
  | otherwise        = False

numbers :: (Num a, Ord a) => a -> a
numbers x
  | x < 0   = -1
  | x == 0  = 0
  | x > 0   = 1

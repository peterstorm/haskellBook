bloodLevels :: Integer -> String
bloodLevels x
  | x > 145 = show x ++ " is too high!"
  | x < 135 = show x ++ " is too low!"
  | otherwise = show x ++ " is just right."


averageGrade :: (Fractional a, Ord a) => a -> Char
averageGrade x
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  | y < 0.59  = 'F'
  where y = x / 100

pal :: Eq a => [a] -> Bool
pal xs
  | xs == reverse xs = True
  | otherwise       = False

numbers :: (Ord a, Num a, Num b) => a -> b
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1

test1 = negate $ sum [1..5]

test2 = negate . sum $ [1..5]

test3 = take 1 . take 5 $ reverse [1..10]

take5Odds :: Integral a => a -> [a]
take5Odds x = take 5 . filter odd . enumFrom $ x

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = d
  where (xLast, _) = divMod x 10
        (_, d)     = divMod xLast 10

hunsDigit :: Integral a => a -> a
hunsDigit x = d
  where (xLast, _) = divMod x 100
        (_, d)     = divMod xLast 10


foldBool :: a -> a -> Bool -> a
foldBool x y bool = case bool of
  True  -> y
  False -> x

foldBool' :: a -> a -> Bool -> a
foldBool' x y bool
  | bool == False = x
  | otherwise     = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)

data Product a b =
  Product a b
  deriving (Eq, Show)


oroductUnpackOnlyA :: Product a b -> a
productUnpackOnlyA (Product x _) = x

productUnpackOnlyB :: Product a b -> b
productUnpackOnlyB (Product _ y) = y

productUnpack :: Product a b -> (a, b)
productUnpack (Product x y) = (x, y)

data SumOfThree a b c = FirstPossible a
                      | SecondPossible b
                      | ThirdPossible c
                      deriving (Eq, Show)

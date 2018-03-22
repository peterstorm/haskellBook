module FunctionComposition where

-- Explaining the need for the `$` operator.
-- Another way to think of it is that it wraps stuff to the left *and* right in parentheses:
-- take 5 . reverse $ [1,2,3,4,5]
-- ===
-- (take 5 . reverse) ([1,2,3,4,5])
-- It goes until the next `$` to the left, while it goes all the way to the right. So:
-- take 5 $ reverse $ [1,2,3]
-- === it has right precedence, so do the rightmos tone
-- take 5 $ (reverse) ([1,2,3])
-- ===
-- (take 5) $ ((reverse) ([1,2,3]))

negateSumList :: Num a => [a] -> a
negateSumList xs =
  negate . sum $ xs

-- pointfree style
negateSumList' :: Num a => [a] -> a
negateSumList' = negate . sum

reverseTake5 :: [a] -> [a]
reverseTake5 xs =
  take 5 . reverse $ xs

-- pointfree style
reverseTake5' :: [a] -> [a]
reverseTake5' = take 5 . reverse

enumFromTake5 :: Enum a => a -> [a]
enumFromTake5 x =
  take 5 . enumFrom $ x

-- pointfree style
enumFromTake5' :: Enum a => a -> [a]
enumFromTake5' = take 5 . enumFrom

filterOddEnum :: (Enum a, Integral a) => a -> [a]
filterOddEnum x =
  take 5 . filter odd . enumFrom $ x

-- pointfree style
filterOddEnum' :: (Enum a, Integral a) => a -> [a]
filterOddEnum' = take 5 . filter odd . enumFrom

filterEvenEnum :: (Enum a, Integral a) => a -> [a]
filterEvenEnum x =
  take 5 . filter even . enumFrom $ x

-- pointfree style
filterEvenEnum' :: (Enum a, Integral a) => a -> [a]
filterEvenEnum' = take 5 . filter even . enumFrom

sumList :: Num a => a -> [a] -> a
sumList = foldr (+)

countLowercaseAs :: String -> Int
countLowercaseAs = length . filter (== 'a')

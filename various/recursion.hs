module Recursion where

import Data.List (intersperse)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n =
  1 + (incTimes (times - 1) n)

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n - 1) f $ b

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes times (+1) n


f :: Bool -> Maybe Int
f False = Just 0
f _     = Nothing

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x =
  fibonacci (x - 1) + fibonacci (x - 2)

-- Explanation of `fibonacci 5`:
-- fib 5 = fib 4 + fib 3
-- fib 5 = (fib 3 + fib 2) + (fib 2 + fib 1)
-- fib 5 = ((fib 2 + fib 1) + (fib 1 + fib 0)) + ((fib 1 + fib 0) + 1)
-- fib 5 = (((fib 1 + fib 0) + 1) + (1 + 0)) + ((1 + 0) + 1)
-- fib 5 = (((1 + 0) + 1) + (1 + 0)) + ((1 + 0) + 1)

-- Integral division from scratch
-- The below or type alises
type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

-- uses `go` notation, where you define a function in a where clause,
-- so we can accept more arguments than the top-level function!
-- also, it uses pattern matching with guards.
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go num denom count
          | num < denom = (count, num)
          | otherwise =
            go (num - denom) denom (count + 1)

-- write a function that recursively sums all numbers from 1 to n
-- where n is the argument.
sumNumbers :: (Eq a, Num a) => a -> a
sumNumbers 0 = 0
sumNumbers x = x + sumNumbers (x - 1)

-- write a function that multiplies two integral numbers using
-- recursive summation.
timesNumbers :: Integral a => a -> a -> a
timesNumbers 0 _ = 0
timesNumbers _ 0 = 0
timesNumbers x y = x + timesNumbers x (y - 1)

-- we're gonna fix dividedBy, so it can handle dividing by 0.
data DividedByResult
  = Result Integer
  | DividedByZero String
  deriving Show

-- we add more guards, and another argument, `neg`, so we can decide
-- if the result should be negated or not.
dividedBy' :: Integer -> Integer -> (DividedByResult, Integer)
dividedBy' num denom = go num denom 0 1
  where go num denom count neg
          | denom == 0  = (DividedByZero "is not allowed", 0)
          | num < 0     = go (negate num) denom count (neg * (-1))
          | denom < 0   = go num (negate denom) count (neg * (-1))
          | num < denom = (Result (count
                            * (if neg == (-1) then (-1) else 1)
                                  ), num)
          | otherwise   = go (num - denom) denom (count + 1) neg

-- define the function MacCarty 91 that yields `x - 10 when x > 100` and `91` otherwise.
mc91 :: Integer -> Integer
mc91 x
  | x > 100   = x - 10
  | otherwise = mc91(mc91 (x + 11))

-- Now we gotta do some `Numbers into words`
digitToWord :: Integer -> String
digitToWord n
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"
  | otherwise = "not a number"

digits :: Integer -> [Integer]
digits x = go x []
  where go x list
          | div x 10 == 0 = (mod x 10) : list
          | otherwise     = go (div x 10) (mod x 10 : list)

wordNumber :: Integer -> String
wordNumber x
  = concat
  . intersperse "-"
  . map digitToWord
  $ digits x

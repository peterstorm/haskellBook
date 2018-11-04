module Scan where

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibs20 :: [Integer]
fibs20 = take 20 f
  where f = 1 : scanl (+) 1 fibs

fibs100 :: [Integer]
fibs100 = [x | x <- f, x < 100]
  where f = 1 : scanl (+) 1 f

factorialScan :: [Integer]
factorialScan = scanl (*) 1 [1..]

factorialScanN :: Int -> [Integer]
factorialScanN n = take n factorialScan


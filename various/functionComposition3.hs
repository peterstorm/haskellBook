module FunctionComposition3 where

print' :: Show a => a -> IO ()
print' = putStrLn . show

-- EXERCISES

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

-- rewrite using `divmod`
tensDigit' :: Integral a => a -> a
tensDigit' x = d
  where (xLast, _) = divMod x 10
        (_, d)     = divMod xLast 10

-- get the hundreds digit
hunsDigit :: Integral a => a -> a
hunsDigit x = d
  where (xLast, _) = divMod x 100
        (_, d)     = divMod xLast 10

-- implement using case
foldBool :: a -> a -> Bool -> a
foldBool x y b =
  case b of
    True  -> y
    False -> x

-- implement using guards
foldBool' :: a -> a -> Bool -> a
foldBool' x y b
  | b          = y
  | otherwise  = x

-- implement using pattern matching
foldBool'' :: a -> a -> Bool -> a
foldBool'' _ y True  = y
foldBool'' x _ False = x

-- fill in the function, using the type signature
g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)
-- if you look at the type signature
-- (a -> b) is a function and the `b` is
-- present in the resulting tuble.
-- Therefor, you must run the function on the
-- value of the first element of the tuple

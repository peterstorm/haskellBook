module Test where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times - 1) n)

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n - 1) f $ b

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes times (+1) n

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go num denom count
          | num < denom = (count, num)
          | otherwise   = go (num - denom) denom (count + 1)

recSum :: (Eq a, Num a) => a -> a
recSum 0 = 0
recSum x = recSum (x - 1) + x

recMult :: Integral a => a -> a -> a
recMult 0 _ = 0
recMult _ 0 = 0
recMult x y = go x y 1 0
  where go x y neg acc
          | x < 0     = if y < 0 then go (negate x) (negate y) 1 0
                                 else go (negate x) y (-1) 0
          | x == 1    = if neg == (-1) then (y + acc) * (-1)
                                       else (y + acc)
          | y == 1    = x + acc
          | otherwise = if neg == (-1) then go (x - 1) y (-1) (acc + y)
                                       else go (x - 1) y 1 (acc + y)

recMult' :: Integral a => a -> a -> a
recMult' 0 _ = 0
recMult' _ 0 = 0
recMult' x y = go x y 1 0
  where
    go x y _ _
        | x < 0, y < 0 = go (negate x) (negate y) 1 0
        | x < 0        = go (negate x) y (-1) 0
    go 1 y (-1) acc = negate (y + acc)
    go 1 y _ acc    = y + acc
    go x 1 _ acc    = x + acc
    go x y neg acc  = go (x - 1) y neg (acc + y)

recMult'' :: Integral a => a -> a -> a
recMult'' 0 _ = 0
recMult'' x y
  | x < 0 = - recMult (-x) y
  | otherwise = y + recMult (x - 1) y

recMult''' :: Integral a => a -> a -> a
recMult''' x y = go (abs x) 0
 where
  y' = if signum x /= signum y then -abs y else abs y
  go 0 acc = acc
  go n acc = go (n - 1) (acc + y')

data DividedResult = Result Integer
                   | DividedByZero
                   deriving Show

fixedDividedBy :: Integral a => a -> a -> DividedResult
fixedDividedBy _ 0 = DividedByZero
fixedDividedBy num denom = go num denom 1 0
  where go num denom neg acc
          | num < 0, denom < 0       = go (abs num) (abs denom) 1 0
          | num < 0                  = go (abs num) denom (-1) 0
          | denom < 0                = go num (abs denom) (-1) 0
          | num < denom, neg == (-1) = Result (-acc)
          | num < denom              = Result acc
          | otherwise                = go (num - denom) denom neg (acc + 1)

mc91 x
  | x > 100     = x - 10
  | otherwise = mc91 (mc91 (x + 11))

main :: IO ()
main = do
  let fac = factorial 4
  putStrLn $ "The result of factorial 4 is: " ++ show fac

  let inc = incTimes 4 5
  putStrLn $ "The result of incTimes 4 5 is: " ++ show inc

  let inc' = incTimes' 10 10
  putStrLn $ "The result of incTimes' 10 10 is: " ++ show inc'

  let apply' = applyTimes 5 (+1) 5
  putStrLn $ "The result of applyTimes 5 (+1) 5 is: " ++ show apply'

  let fibo = fibonacci 6
  putStrLn $ "The 6th number in the Fibonacci sequence is: " ++ show fibo

  let divided = dividedBy 120 26
  putStrLn $ "The (quotient, remainder) of dividedBy 120 26 is: " ++ show divided

  let summed = recSum 5
  putStrLn $ "The result of recSum 5 is: " ++ show summed

  let multed = recMult 10 (-10)
  putStrLn $ "The result of recMult 10 -10 is: " ++ show multed

  let fixedDiv = fixedDividedBy 10 (-2)
  putStrLn $ "The rusult of fixedDividedBy 10 (-2) is: " ++ show fixedDiv

  let macMac = map mc91 [95..110]
  putStrLn $ "The result of map mc91 [95..100] is: " ++ show macMac

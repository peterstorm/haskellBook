module TupleFunctions where

-- using pattern matching
addEmUp :: Num a => (a, a) -> a
addEmUp (x, y) = x + y

-- using the normal inbuilt functions
addEmUpAlt :: Num a => (a, a) -> a
addEmUpAlt x = (fst x) + (snd x)

-- again using pattern matching
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, x) = x


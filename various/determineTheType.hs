{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

-- a
a = (* 9) 6

-- b
b = head [(0, "doge"), (1, "kitteh")]

-- c
c = head [(0 :: Integer, "doge"), (1, "kitteh")]

-- d
d = if False then True else False

-- e
e = length [1,2,3,4,5]

-- f
f = (length [1,2,3,4]) > (length "TACOCAT")

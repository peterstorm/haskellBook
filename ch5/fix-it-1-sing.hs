-- module sing where

-- module names are capitalised
module Sing where

-- fstString :: [Char] ++ [Char]
-- type signatures are not where operations are declared
fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"


-- sndString :: [Char] -> Char
-- we can't return a single Char with the function declaration we have
sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

-- sing = if (x > y) then fstString x or sndString y
-- if then else
sing = if (x > y) then fstString x else sndString y
                  where x = "Singin"
                        y = "Somewhere"

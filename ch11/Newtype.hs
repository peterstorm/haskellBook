{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}

module Newtype where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

-- define the Goats instance to behave different than the underlying type Int.
--instance TooMany Goats where
--  tooMany (Goats n) = n > 42

instance TooMany (Int, String) where
  tooMany (n, _) = tooMany n

--instance TooMany (Int, Int) where
--  tooMany (x, y) = tooMany (x + y)

instance (Num a, Ord a) => TooMany (a, a) where
  tooMany (x, y) = (x + y) > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

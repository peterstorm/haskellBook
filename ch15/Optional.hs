module Optional where

import Data.Monoid

data Optional a = Nada
                | Only a
                deriving (Eq, Show)

-- apparently we have to derive Semigroup in 8.3.x, for mappend, instead of Monoid?
instance Semigroup a => Semigroup (Optional a) where
  (<>) Nada Nada         = Nada
  (<>) (Only a) Nada     = Only a
  (<>) Nada (Only a)     = Only a
  (<>) (Only a) (Only b) = Only (a <> b)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

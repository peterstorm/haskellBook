module MonoidTests where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

data Bull = Fools
          | Twoo
          deriving (Eq, Show)

data Optional a = Nada
                | Only a
                deriving (Eq, Show)

newtype First' a = 
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada

instance Semigroup (First' a) where
  (<>) (First' Nada) (First' Nada)    = First' Nada
  (<>) (First' (Only a)) (First' Nada) = First' (Only a)
  (<>) (First' Nada) (First' (Only a)) = First' (Only a)
  (<>) (First' (Only a)) (First' (Only b)) = First' (Only a)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = genFirst

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools)
                        , (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools

instance Semigroup Bull where
  (<>) _ _ = Fools

-- apparently we have to derive Semigroup in 8.3.x, for mappend, instead of Monoid?
instance Semigroup a => Semigroup (Optional a) where
  (<>) Nada Nada         = Nada
  (<>) (Only a) Nada     = Only a
  (<>) Nada (Only a)     = Only a
  (<>) (Only a) (Only b) = Only (a <> b)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = genOptional

type BullMappend = Bull -> Bull -> Bull -> Bool

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

genFirst :: Arbitrary a => Gen (First' a)
genFirst = do
  a <- arbitrary
  return $ First' {getFirst' = a}

genOptional :: Arbitrary a => Gen (Optional a)
genOptional = do
  a <- arbitrary
  frequency [ (1, return Nada)
            , (10, return $ Only a) ]

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

main :: IO ()
main = do
  let ma = monoidAssoc
      mli = monoidLeftIdentity
      mri = monoidRightIdentity
  quickCheck (ma :: FirstMappend)
  quickCheck (mli :: FstId)
  quickCheck (mri :: FstId)



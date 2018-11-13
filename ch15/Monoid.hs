module Monoid where

import Test.QuickCheck
import Semigroup


newtype Mem s a =
  Mem { runMem :: s -> (a, s) }

instance Semigroup a => Semigroup (Mem s a) where
  (<>) (Mem f) (Mem g) = Mem $ combine f g
    where combine f g x = let (a, b) = g x
                              (c, d) = f b
                          in (a <> c, d)

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend = (<>)

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)


instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
  mempty = Combine mempty

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

main :: IO ()
main = do
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (monoidAssoc:: TrivAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  quickCheck (monoidAssoc:: IdentAssoc)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)
  quickCheck (monoidAssoc :: TwoAssoc) 
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (monoidAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  quickCheck (monoidAssoc :: BoolDisjAssoc)

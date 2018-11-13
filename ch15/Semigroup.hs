{-# LANGUAGE RankNTypes #-}

module Semigroup where

import Test.QuickCheck hiding (Success,Failure)

data Trivial = Trivial deriving (Eq, Show)

newtype Identity a = Identity a deriving (Eq, Show)

data Two a b = Two a b deriving (Eq, Show)

data Three a b c = Three a b c deriving (Eq, Show)

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

data Or a b = Fst a
            | Snd b
            deriving (Eq, Show)

newtype Combine a b = 
  Combine { unCombine :: (a -> b) }

newtype Comp a = Comp { unComp :: (a -> a) }

data Validation a b = Failure a
                    | Success b
                    deriving (Eq, Show)

instance Semigroup Trivial where
  (<>) _ _ = Trivial

instance Semigroup a => Semigroup (Identity a) where
  (<>) (Identity a) (Identity b) = Identity (a <> b)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two a b) (Two a' b') = Two (a <> a') (b <> b')

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (<>) (Three a b c) (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance Semigroup BoolConj where
  (<>) (BoolConj False) _ = BoolConj False
  (<>) _ (BoolConj False) = BoolConj False
  (<>) _ _                = BoolConj True

instance Semigroup BoolDisj where
  (<>) (BoolDisj True) _                = BoolDisj True
  (<>) _ (BoolDisj True)                = BoolDisj True
  (<>) (BoolDisj False) (BoolDisj False) = BoolDisj False

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
  (<>) (Snd b) (Snd b') = Snd b
  (<>) (Snd b) _        = Snd b
  (<>) _ (Snd b)        = Snd b
  (<>) (Fst a) (Fst a') = Fst a

instance Semigroup b => Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) = Combine $ \x -> f x <> g x 

instance Semigroup a => Semigroup (Comp a) where
  (<>) (Comp f) (Comp g) = Comp $ f . g

instance (Semigroup a, Semigroup b) => Semigroup (Validation a b) where
  (<>) (Success b) (Success b') = Success b
  (<>) (Failure a) (Failure a') = Failure (a <> a')
  (<>) (Success b) _            = Success b
  (<>) _ (Success b)            = Success b

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen
    where identityGen :: Arbitrary a => Gen (Identity a)
          identityGen = do
            a <- arbitrary
            return $ Identity a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen
    where twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
          twoGen = do
            a <- arbitrary
            b <- arbitrary
            return $ Two a b


instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = threeGen
    where threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
          threeGen = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            return $ Three a b c

instance Arbitrary BoolConj where
  arbitrary = boolConjGen
    where boolConjGen :: Gen BoolConj
          boolConjGen = elements [BoolConj False, BoolConj True]

instance Arbitrary BoolDisj where
  arbitrary = boolDisjGen
    where boolDisjGen :: Gen BoolDisj
          boolDisjGen = elements [BoolDisj False, BoolDisj True]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = orGen
    where orGen :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
          orGen = do
            a <- arbitrary
            b <- arbitrary
            frequency [ (1, return $ Fst a)
                      , (1, return $ Snd b) ]

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return $ Failure a)
              , (4, return $ Success b) ]


type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool
type IdentAssoc = Identity String -> Identity String -> Identity String -> Bool 
type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool
type ThreeAssoc = Three String String String -> Three String String String -> Three String String String -> Bool
type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
type OrAssoc = Or String String -> Or String String -> Or String String -> Bool
type ValAssosc = Validation String String -> Validation String String -> Validation String String -> Bool
type CombineAssoc = Combine Int String -> Combine Int String -> Combine Int String -> Int -> Bool

type FunctionAssociativity x c = x -> x -> x -> c -> Bool

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

prop_combAssoc :: (Eq b, Semigroup b) => FunctionAssociativity (Combine a b) a
prop_combAssoc f g h c = unCombine ((f <> g) <> h) c == unCombine (f <> (g <> h)) c


--main :: IO ()
--main = do
  --quickCheck (semigroupAssoc :: TrivAssoc)
  --quickCheck (semigroupAssoc :: IdentAssoc)
  --quickCheck (semigroupAssoc :: TwoAssoc)
  --quickCheck (semigroupAssoc :: ThreeAssoc)
  --quickCheck (semigroupAssoc :: BoolConjAssoc)
  --quickCheck (semigroupAssoc :: BoolDisjAssoc)
  --quickCheck (semigroupAssoc :: OrAssoc)
  --quickCheck (semigroupAssoc :: ValAssosc)

module FunctorInstances where

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Eq (f a), Functor f) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
                     f a
                  -> Fun a b
                  -> Fun b c
                  -> Bool
functorCompose x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type ListOfInt = [Int]
type IntToInt = Fun Int Int
type IntFC = ListOfInt -> IntToInt -> IntToInt -> Bool

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentityInt = Identity Int
type IdentityFC = IdentityInt -> Fun Int Int -> Fun Int Int -> Bool

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return $ Pair a a

type PairInt = Pair Int
type PairFC = PairInt -> Fun Int Int -> Fun Int Int -> Bool

data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoInt = Two Int Int
type TwoFC = TwoInt -> Fun Int Int -> Fun Int Int -> Bool

data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type ThreeInt = Three Int Int Int
type ThreeFC = ThreeInt -> Fun Int Int -> Fun Int Int -> Bool

data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    pure $ Three' a b b

type ThreeInt' = Three' Int Int
type ThreeFC' = ThreeInt' -> Fun Int Int -> Fun Int Int -> Bool

main :: IO ()
main = do
  quickCheck (functorIdentity :: ListOfInt -> Bool)
  quickCheck (functorCompose :: IntFC)
  putStrLn "Identity test:"
  quickCheck (functorIdentity :: IdentityInt -> Bool)
  quickCheck (functorCompose :: IdentityFC)
  putStrLn "Pair test:"
  quickCheck (functorIdentity :: PairInt -> Bool)
  quickCheck (functorCompose :: PairFC)
  putStrLn "Two test:"
  quickCheck (functorIdentity :: TwoInt -> Bool)
  quickCheck (functorCompose :: TwoFC)
  putStrLn "Three test:"
  quickCheck (functorIdentity :: ThreeInt -> Bool)
  quickCheck (functorCompose :: ThreeFC)
  putStrLn "Three' test:"
  quickCheck (functorIdentity :: ThreeInt' -> Bool)
  quickCheck (functorCompose :: ThreeFC')




# Chapter 15 - Functor

```haskell
{-# LANGUAGE FlexibleInstances #-}

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

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    pure $ Four a b c d

type FourInt = Four Int Int Int Int
type FourFC = FourInt -> Fun Int Int -> Fun Int Int -> Bool

data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    pure $ Four' a a a b

type FourInt' = Four' Int Int
type FourFC' = FourInt' -> Fun Int Int -> Fun Int Int -> Bool

data Possibly a = LolNope
                | Yeppers a
                deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope     = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

instance Arbitrary a => Arbitrary (Possibly a) where
  arbitrary = do
    a <- arbitrary
    frequency [ (1, pure LolNope)
              , (4, pure $ Yeppers a) ]

type PossiblyInt = Possibly Int
type PossiblyFC = PossiblyInt -> Fun Int Int -> Fun Int Int -> Bool

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a)  = First a
  fmap f (Second b) = Second (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, pure $ First a)
              , (4, pure $ Second b) ]

type SumInt = Sum Int Int
type SumFC = SumInt -> Fun Int Int -> Fun Int Int -> Bool

data Wrap f a =
  Wrap (f a)
  deriving (Eq, Show)

data Quant a b = Finance
               | Desk a
               | Bloor b
               deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk a)  = Desk a
  fmap f (Bloor b) = Bloor $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, pure $ Finance)
              , (4, pure $ Desk a) 
              , (5, pure $ Bloor b) ]

type QuantInt = Quant Int Int
type QuantFC = QuantInt -> Fun Int Int -> Fun Int Int -> Bool

newtype K a b = K a
  deriving (Eq, Show)

newtype Flip f a b = Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K (f a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (K a b) where
  arbitrary = do
    a <- arbitrary
    pure $ K a

data EvilGoateeConst a b =
  GoatyConst b
  deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
  arbitrary = do
    b <- arbitrary
    pure $ GoatyConst b

type EvilGoateeConstInt = EvilGoateeConst Int Int
type EvilGoateeConstFC = EvilGoateeConstInt -> Fun Int Int -> Fun Int Int -> Bool

data LiftItOut f a =
  LiftItOut (f a)
  deriving (Eq, Show)


instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

instance Arbitrary a => Arbitrary (LiftItOut Maybe a) where
  arbitrary = do
    a <- arbitrary
    pure $ LiftItOut $ Just a

type LiftItOutMaybeInt = LiftItOut Maybe Int
type LiftItOutFC = LiftItOutMaybeInt -> Fun Int Int -> Fun Int Int -> Bool

data Parappa f g a =
  DaWrappa (f a) (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa (fa) (ga)) = DaWrappa (fmap f fa) (fmap f ga)

instance Arbitrary a => Arbitrary (Parappa Maybe Maybe a) where
  arbitrary = do
    a <- arbitrary
    pure $ DaWrappa (Just a) (Just a)

type ParappaMaybeInt = Parappa Maybe Maybe Int
type ParappaFC = ParappaMaybeInt -> Fun Int Int -> Fun Int Int -> Bool

data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)
  deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

data Notorious g o a t =
  Notorious (g o) (g a) (g t)
  deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

instance Functor (List) where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

data GoatLord a = NoGoat 
                | OneGoat a 
                | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat            = NoGoat
  fmap f (OneGoat a)       = OneGoat (f a)
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt        = Halt
  fmap f (Print x y) = Print x (f y)
  fmap f (Read g)    = Read $ fmap f g


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
  putStrLn "Four test:"
  quickCheck (functorIdentity :: FourInt -> Bool)
  quickCheck (functorCompose :: FourFC)
  putStrLn "Four' test:"
  quickCheck (functorIdentity :: FourInt' -> Bool)
  quickCheck (functorCompose :: FourFC')
  putStrLn "Possibly test:"
  quickCheck (functorIdentity :: PossiblyInt -> Bool)
  quickCheck (functorCompose :: PossiblyFC)
  putStrLn "Sum test:"
  quickCheck (functorIdentity :: SumInt -> Bool)
  quickCheck (functorCompose :: SumFC)
  putStrLn "Quant test:"
  quickCheck (functorIdentity :: QuantInt -> Bool)
  quickCheck (functorCompose :: QuantFC)
  putStrLn "K test: I don't know how to implemement this test"
  putStrLn "EvilGoateeConst test:"
  quickCheck (functorIdentity :: EvilGoateeConstInt -> Bool)
  quickCheck (functorCompose :: EvilGoateeConstFC)
  putStrLn "LiftItOut test:"
  quickCheck (functorIdentity :: LiftItOutMaybeInt -> Bool)
  quickCheck (functorCompose :: LiftItOutFC)
  putStrLn "Parappa test:"
  quickCheck (functorIdentity :: ParappaMaybeInt -> Bool)
  quickCheck (functorCompose :: ParappaFC)
```

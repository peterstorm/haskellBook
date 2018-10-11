# Chapter 6 - Typeclasses

## 6.2 What are typeclasses?

- a type declaration defines how a type is created
- a typeclass defines _how a set of types are used in computations_.

A typeclass allows different data types to make use of the same set of functions
without redefining them, e.g. equality (`Eq`) is useful not only to numeric values,
but to many other types, too.

When using a typeclass method on a type that has an instance of that typeclass,
the compiler looks up how the function works for that type.

## 6.4 Eq

Haskell does not encode equality into every type.

```haskell
-- (==) is constrained to types that have an instance of Eq
:t (==)
(==) :: Eq a => a -> a -> Bool

-- this can be further constrained by partially applying a type
:t (==) 5
(==) 5 :: (Eq a, Num a) => a -> Bool
```

_Deriving_ refers to not having to manually implement instances of typeclasses
when creating new data types.

e.g. when we use `deriving Show` to automatically allow a datatype to be printed
to the screen.

## 6.5 Writing typeclass instances

`Eq` can simply be derived when creating a new datatype, but it's one of the
easiest to write instances for.

### Eq instances

In the documentation for `Eq` we see the following statement:

*Minimal complete definition: either == or /=*

_Minimal complete definition_ refers to the minimum set of methods from `Eq`
that your new datatype must implement in order to have an instance of a
valid instance of a typeclass.

```haskell
-- given
data Trivial = Trivial

-- we can't use equality because Trivial does not have an instance of Eq
Trivial == Trivial
-- throws 'No instance for (Eq Trivial)'
```

#### Trivial datatype


```haskell
-- we use the ' here to differentiate between the type and data constructor
data Trivial =
  Trivial'

instance Eq Trivial where
-- [1]   [2]  [3]    [4]
  Trivial' == Trivial' = True
--  [5]   [6]   [7]       [8]
-- or
-- (==) Trivial' Trivial' = True
-- [            9              ]

-- [1] typeclass instance - how a typeclass works for a specific datatype
-- [2] the typeclass for this instance
-- [3] the datatype the instance is for
-- [4] 'where' terminates the initial declaration and beginning of the instance
-- [5] the first argument to (==)
-- [6] the function (==) in infix position
-- [7] the second argument to (==)
-- [8] the result of application of (==) to the arguments
-- [9] the same expression written using prefix position

Prelude> Trivial' == Trivial'
True
```

#### DayOfWeek and Date datatypes

```haskell
data DayOfWeek
  Mon | Tue | Wed | Thu | Fri | Sat | Sun

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  -- this is the unconditional case - without it, (==) on DayOfWeek is unsafe
  (==) _ _ = False

data Date =
  Date DayOfWeek Int

instance Eq Date where
  (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth')
    = weekday == weekday' && dayOfMonth == dayOfMonth'
```

#### Partial functions --- not so strange danger

_Partial functions_ are functions that don't handle all possible cases,
resulting in situations where the code will not evaluate.

Examples of this are `head` and `tail`:

```haskell
head []
-- throws

tail []
-- throws
```

Partial functions should be avoided, and can be replaced by safe variations:

```haskell
head []

-- can be made safe with (although the type returned is List)
take 1 []

tail []

-- can be made safe as follows:
drop 1 []
```

In our `DayOfWeek` example, if we had omitted the unconditional case:

```haskell
  (==) _ _ = False
```

we can't evaluate the following:

```haskell
Mon == Tue
*** Exception: ...: Non-exhaustive patterns in function ==
```

The unconditional case handles arguments in the typeclass instance handles every
possible situation that we haven't specified a behaviour for.

In Prelude we can ensure that when we are not handling all cases by using `Wall`:

```haskell
Prelude>:set -Wall
Prelude>:l DayOfWeek.hs
DayOfTheWeek.hs:5:3: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for ‘==’:
        Patterns not matched:
            Mon Tue
            Mon Wed
            Mon Thu
            Mon Fri
            ...

-- mind blown...
```

#### Sometimes we need to ask for more

Types with polymorphic parameters will sometimes require a typeclass to be
specified in the typeclass instance.

An incomplete example:

```haskell
data Identity a =
  Ident a

instance Eq (Identity a) where
  (==) (Ident v) (Ident v') = v == v'

-- this will not typecheck, because in order to evaluate v == v' we need both v
-- and v' to have an instance of Eq
```

A complete example:

```haskell
data Identity a =
  Ident a

instance Eq a => Eq (Identity a) where
         [  1  ]
  (==) (Ident v) (Ident v') = v == v'
-- [1] 'a' must have an instance of Eq
```

#### Exercises: Eq instances

1.
    ```haskell
      -- given
      data TisAnInteger =
        TisAn Integer

      -- then
      instance Eq TisAnInteger where
        (==) (TisAn a) (TisAn a') = a == a'

      -- no need to set a constraint on the typeclass instance, Integer is
      -- concrete
    ```

2.
    ```haskell
      -- given
      data TwoIntegers =
        Two Integer Integer

      -- then
      instance Eq TwoIntegers where
        (==) (Two a b) (Two a' b') = a == a' && b == b'
    ```

3.
    ```haskell
      -- given
      data StringOrInt =
        TisAnInt Int | TisAString String

      -- then
      instance Eq StringOrInt where
        (==) (TisAnInt a) (TisAnInt a') = a == a'
        (==) (TisAString a) (TisAString a') = a == a'
        (==) _ _ = False
    ```

4.
    ```haskell
      -- given
      data Pair a =
        P a

      -- then
      instance Eq a => Eq (Pair a) where
        (==) (P p) (P p') = p == p'

      -- we need the typeclass constraint on 'a' here because it's possible to
      -- pass in a value for 'a' that doesn't implement Eq. The typeclass
      -- constraint ensures we can never do that
    ```

5.
    ```haskell
      -- given
      data Tuple a b =
        T a b

      -- then
      instance (Eq a, Eq b) => Eq (Tuple a b) where
        (==) (T v1 v2) (T v1' v2') = v1 == v1' && v2 == v2'

      -- again, we need both arguments to have an instance of Eq
    ```

6.
    ```haskell
      -- given
      data Which a =
        ThisOne a
        | ThatOne a

      -- then
      instance Eq a => Eq (Which a) where
        (==) (ThisOne a) (ThisOne a') = a == a'
        (==) (ThisOne a) (ThatOne a') = a == a'
        (==) (ThatOne a) (ThatOne a') = a == a'
        (==) _ _ = False
    ```

7.
    ```haskell
      -- given
      data EitherOr a b =
        Hello a
        | Goodbye b

      -- then
      instance (Eq a, Eq b) => Eq (EitherOr a b) where
        (==) (Hello v) (Hello v') = v == v'
        (==) (Goodbye v) (Goodbye v') = v == v'
        (==) _ _ = False
    ```

## 6.6 Num

### Integral

We have

```haskell
:i Integral
class (Real a, Enum a) => Integral a where
  ...
```

So we know that for an integral method to be applied to `a` it must have an
instance of both `Real` and `Enum`.

Now from

```haskell
:i Real
class (Num a, Ord a) => Real a where
  ...
```

we know that `Real` implements `Num`. `Integral` has all of `Real`s methods
available to it, and all of `Num`s methods available to it. Furthermore, because
typeclass methods can't be overridden we know we'll never have any ambiguity as
to what a method may do.

This type of inheritance is _additive_.

#### Exercises: Tuple Experiment

```haskell
:i quotRem
-- from Integral
quotRem :: a -> a -> (a, a)

-- we know quotRem takes 2 Integral values, and returns a tuple containing 2
-- Integral values

quotRem 10 5
(2, 0)

quotRem 5 10
(0, 5)

quotRem 9 4
(2, 1)

quotRem 4 9
(0, 4)

-- -9 / 4 == -2.25
quotRem -9 4
(-2, -1)

quotRem 9 (-4)
(-2, 1)

-- integral division rounding towards 0
-- if numerator is negative, return the +ve equivalent of the operation with
-- both values negative
-- if the denominator is negative, return only the quotient value -ve
```

```haskell
:i divMod
-- from Integral
divMod :: a -> a -> (a, a)

divMod 10 5
(2, 0)

divMod 5 10
(0, 5)

divMod 9 4
(2, 1)

divMod 4 9
(0, 4)

-- -9 / 4 == -2.25
divMod -9 4
(-3, 3)

divMod 9 (-4)
(-3, -3)

-- integral division with modulus
-- rounding down to -Infinity
-- adding the result of the modulus to the divisor

-- e.g.
-- -9 / 4 = -2.25
-- => -3
-- -9 mod 4 = -1
-- => 4 + (-1) = 3
-- => divMod (-9) 4 == (-3, 3)
```

## 6.7 Type-defaulting typeclasses

When evaluating polymorphic constrained values, the compiler resolves them to
concrete types. The concrete type must have all the type instances as specified
by the constraints of the function / datatype.

Concrete types are usually inferred from a type signature, but not all terms
will have a type signature to refer to to obtain a concrete type:

```haskell
(+) 1 2 :: Integer
-- where does Integer come from?
```

The [Haskell Report](https://www.haskell.org/onlinereport/haskell2010/) defines the
default concrete types that typeclasses default to:

```haskell
default Num Integer
default Real Integer
default Enum Integer
default Integral Integer
default Fractional Double
default RealFrac Double
default Floating Double
default RealFloat Double
```

This is called _type defaulting_.

```haskell
(/) :: Fractional a => a -> a -> a

-- defaults to
(/) :: Double -> Double -> Double
```

We can specify monomorphic functions by specifying the type we expect values to
implement:

```haskell
intAdd = (+) :: Int -> Int -> Int

numId = id :: Num a => a -> a
intId = id :: Int a => a -> a
```

Types can be made more specific, but not more general or polymorphic.

## 6.8 Ord

`Ord` is constrained by `Eq` -> you need a way to determine if values are equal
if you are going to be able to order them:

```haskell
:i Ord
class Eq a => Ord a where
  ...
```

`True` is greater than `False` because of how `Bool` is defined:

```haskell
data Bool = False | True

-- data constructors declared later in a data definition are considered greater
-- than those declared before them
compare True False
GT
```

### Ord instances

`Ord` instances rely on the way the datatype is defined:

```haskell
data DayOfWeek
  Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Ord, Show)

Prelude> Mon > Tue
False
```

Values to the left of a data definition are less than values to the right.

This can be overridden with the typeclass instance:

```haskell
data DayOfWeek
  Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Ord, Show)

-- make Fri always greater than other days of the week, and other days all equal
instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _ = GT
  compare _ Fri = LT
  compare _ _ = EQ

compare Fri Mon
GT

compare Mon Tue
EQ

-- because we declared Eq (==) will still apply regardless of Ord's instance
Mon == Tue
False

-- this is weird, through
```

Despite what we can do with Ord and Eq, Ord instances should always agree with Eq.

### Ord implies Eq

In the following:

```haskell
check' :: Ord a => a -> a -> Bool
check' a b = (==) a b
```

`(==)` would usually require an instance `Eq` of as a constraint. This is not
required, because `Ord` implies that constraint on type variables already. `Eq`
is thus implied as a result of the `Ord` constraint:

```haskell
-- Ord requires type variables to implement Eq, so we get all the methods
-- available to type variables that implement Eq
-- Eq is a superclass of Ord
:i Ord
class Eq a => Ord a where
  ...
```

It's not encouraged to write like this, and we would rather use the minimal
sufficient `Eq`, but the example at least demonstrates superclasses.

### Exercises: Will they work?

1.
    ```haskell
      max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])
      -- will work
      -- in this case we have
      max :: Int -> Int -> Int

      -- and Int has an instance of Ord
    ```

2.
    ```haskell
      compare (3 * 4) (3 * 5)
      -- will work
      -- we have
      compare :: Integer -> Integer -> Integer

      -- and Integer has an instance of Ord
    ```

3.
    ```haskell
      compare "Julie" True
      -- will not work because although "Julie" and True both have instances of
      -- Ord, they are not of the same type
      -- compare expects [Char] on the second argument, but receives a Bool
    ```

4.
    ```haskell
      (5 + 3) > (3 + 6)
      -- will work
    ```

## 6.9 Enum

`Enum`s are types that are enumerable, and therefore have successors and
predecessors.

Numbers and letters have successors and predecessors:

```haskell
succ 5
6

pred 'b'
'a'
```

`Enum` can be used to build lists of enumerable values.

## 6.10 Show

GHCi uses `Show` to create `String` values it can print in the terminal.

Printing is a side effect.

```haskell
:t print
print :: Show a -> a -> IO ()

-- print takes an 'a', and returns an IO () result. IO is an action that returns
-- a value of the type () - unit
```

`main` returns the type `IO ()` because it _only_ produces side effects.

An `IO` action is an action that, when performed, produces side effects such as
reading from input, printing to the screen, writing to files, etc.

Because expressions in Haskell can't not return anything, `IO` returns `()`.

A type of `IO String` means that we have performed side effects on the way to
producing that `String` value.

```haskell
-- a normal String value
myVal :: String

-- a value of String that has been produced
-- as a result of performing side effects
myIoString :: IO String
```

All invoking of `print` results in IO, whether implicit (through `Show`) or
explicit.

### Working with `Show`

`Show` is required in order for anything to be printed to the terminal.

A minimal implementation of `Show`:

```haskell
data Mood = Blah

instance Show Mood where
  show _ = "Blah"

Prelude> Blah
Blah
```

## 6.11 Read

`Read` should be avoided because it is partial:

```haskell
-- this works
read "1234" :: Integer
1234

-- this doesn't
read "abc" : Integer
*** Exception: Prelude.read: no parse
```

For this reason we avoid `Read` entirely - avoid partial functions.

## 6.12 Instances are dispatched by type

_Typeclass instances_ are unique pairing of typeclasses and the types that
implement the methods in those typeclasses.

- a typeclass defines functions and/or values
- types have instances for those typeclasses
- the instances specify the ways a type will use functions in that typeclass

Do not write typeclasses that do not define any functions:

```haskell
-- BAD
-- no definition of how these functions work - only their signatures
-- Numberish doesn't actually do anything
class Numberish a where
  fromNumber:: Integer -> a
  toNumber:: a -> Integer

newtype Age =
  Age Integer
  deriving (eq, Show)

-- we define how Numberish is implemented only in the Age datatype
instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n
```

Don't create default values using typeclasses:

```haskell
class Numberish where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer
  defaultNumber :: a

-- defaultNumber is now available wherever Numberish is available, but Numberish
-- has no indication of what type defaultNumber is. It could be anything.
-- Attempting to access defaultNumber in the REPL will throw an error because it
-- knows nothing apart from it being provided for by instances of Numberish
```

## 6.13 Gimme more operations

Adding typeclass constraints to functions that only accept concrete types
is redundant:

```haskell
myIntCheck :: Ord, Int => Int -> Int -> Int
myIntCheck a b = a > b
```

`Int` already implements `Ord`, so we get no additional meaning from adding the
constraint.

Note: Using concrete types means we're working with types that have many
inhabitants, typeclasses, and operations. The less specific we are, i.e. by
using more polymorphic types, the fewer opportunities there are to introduce
mistakes through a larger surface area. It is generally better to avoid concrete
types in favour of more polymorphic types, resulting in a smaller surface area
for errors.

## 6.14 Exercises

### Multiple choice

1. The `Eq` class: makes equality tests possible
2. The typeclass `Ord`: is a subclass of `Eq` (because types implementing `Ord`
   must have an instance of `Eq`)
3. Suppose the typeclass `Ord` has an operator `>`. What is the type of `>`?
    ```haskell
      :t (>)
      (>) :: Ord a => a -> a -> Bool
    ```
4. In `x = divMod 16 12`: the type of `x` is a tuple
5. The typeclass `Integral` includes: `Int` and `Integer` numbers

### Does it typecheck?

1.
    ```haskell
      -- given
      data Person = Person Bool

      printPerson :: Person -> IO ()
      printPerson person = putStrLn( show person )

      -- does not typecheck - Person does not derive Show, so show will not
      -- accept person as an argument
    ```

    ```haskell
      -- fixed
      data Person = Person Bool deriving Show

      printPerson :: Person -> IO ()
      printPerson person = putStrLn( show person )
    ```

2.
    ```haskell
      -- given
      data Mood = Blah
        | Woot deriving Show

      settleDown x = if x == Woot
                      then Blah
                      else x

      -- does not typecheck because we are evaluating using equality, but Mood
      -- does not implement Eq
    ```

    ```haskell
      -- fixed
      data Mood = Blah
        | Woot deriving (Show, Eq)

      settleDown x = if x == Woot
                      then Blal
                      else x
    ```

3.
    a.) values that inhabit the data definition of `Mood` - `Blah` and `Woot`.
    In fact, the type of `settleDown` is `settleDown :: Mood -> Mood`
    b.) an error is thrown showing `Mood` does not have an instance of `Num`
    c.) we would get an error because `Mood` does not implement `Ord`

4.
    ```haskell
      -- given
      type Subject = String
      type Verb = String
      type Object = String

      data Sentence =
        Sentence Subject Verb Object
        deriving (Ord, Show)

      s1 = Sentence "dogs" "drool"
      s2 = Sentence "Julie" "loves" "dogs"

      -- typechecks
    ```

### Given a datatype, what can we do?

```haskell
-- given
data Rocks =
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show)
```

1.
    ```haskell
    phew = Papu "chases" True

    -- does not typecheck
    -- Papu expects a type of Rocks, not string
    -- Papu expects a type of Yeah, not Bool
    :t Papu
    Papu :: Rocks -> Yeah -> Papu
    ```

2.
    ```haskell
      truth = Papu (Rocks "chomskydoz")
                   (Yeah True)

      -- typechecks
    ```

3.
    ```haskell
      equalityForAll :: Papu -> Papu -> Bool
      equalityForAll p p' = p == p'

      -- typechecks
    ```

4.
    ```haskell
      comparePapus :: Papu -> Papu -> Bool
      comparePapus p p' = p > p'

      -- does not typecheck
      -- Papu needs to implement Ord in order to typecheck
    ```

### Match the types

1. 
    a.)
    ```haskell
      i :: Num a => a
      i = 1
    ```
    b.)
    ```haskell
      i :: a

      -- can't be substituted
      -- a needs to be at least type Num
      -- why? error messages says:
      -- "Couldn't match type 'a' with actual type Num"
      -- Does it throw because 'a' is parametrically polymorphic, and the
      -- compiler expects at least some constraint when we do nothing with the
      -- value?
    ```

2. 
    a.)
    ```haskell
      f :: Float
      f = 10
    ```
    b.)
    ```haskell
      f :: Num a => a

      -- can be substituted
    ```

3. 
    a.)
    ```haskell
      f :: Float
      f = 1.0
    ```
    b.)
    ```haskell
      f :: Fractional a => a

      -- will substitute - Float implements Fractional
    ```

4. 
    a.)
    ```haskell
      f :: Float
      f = 1.0
    ```
    b.)
    ```haskell
      f :: RealFrac a => a

      -- will substitute - Float implements RealFrac
    ```

5. 
    a.)
    ```haskell
      freud :: a -> a
      freud x = x
    ```
    b.)
    ```haskell
      freud :: Ord a => a -> a

      -- will substitute - we're increasing specificity on a parametrically
      -- polymorphic function
    ```

6. 
    a.)
    ```haskell
      freud' :: a -> a
      freud' x = x
    ```
    b.)
    ```haskell
      freud' :: Int -> Int

      -- will substitute - for the same reasons in 5.
    ```

7. 
    a.)
    ```haskell
      myX = 1 :: Int

      sigmund :: Int -> Int
      sigmund x = myX
    ```
    b.)
      ```haskell
        sigmund :: a -> a

        -- will not substitute - myX is always Int, but sigmund expects to
        -- return any type
      ```

8. 
    a.)
    ```haskell
      myX = 1 :: Int

      sigmund' :: Int -> Int
      sigmund' x = myX
    ```
    b.)
    ```haskell
      sigmund' :: Num a => a -> a

      -- will not substitute - although Int implements Num, sigmund' expects
      -- a return value of anything that implements Num, not only Int
      -- a fix:
      -- sigmund' :: Num a => a -> Int
    ```

9. 
    a.)
    ```haskell
      jung :: Ord a => [a] -> a
      jung xs = head (sort xs)
    ```
    b.)
    ```haskell
      jung :: [Int] -> Int

      -- will substitute - Int implements Ord
    ```

10. 
    a.)
    ```haskell
      young :: [Char] -> Char
      young xs = head (sort xs)
    ```
    b.)
    ```haskell
      young :: Ord a => [a] -> a

      -- will substitute - this is a less constrained variation of the original
    ```

11. 
    a.)
    ```haskell
      mySort :: [Char] -> [Char]
      mySort = sort

      signifier :: [Char] -> Char
      signifier xs = head (mySort xs)
    ```
    b.)
    ```haskell
      signifier :: Ord a => [a] -> a

      -- will not substitute - mySort expects [Char], while signifier will
      -- accept any list
    ```

### Type-Kwon-Do Two: Electric Typealoo

1.
    ```haskell
      -- given
      chk :: Eq b => (a -> b) -> a -> b -> Bool

      -- then
      chk f x y = f x == y
    ```

2.
    ```haskell
      -- given
      arith :: Num b => (a -> b) -> Integer -> a -> b

      -- then
      arith f int x = (f x) + (fromIntegral int)
    ```

## 6.15 Chapter definitions

1. _Typeclass inheritance_ is when a typeclass has a superclass. Subclasses
   inherit from their superclasses.

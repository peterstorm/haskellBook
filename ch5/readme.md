# Chapter 5 - Types

## 5.1 Types

As we have seen, a datatype declaration defines a type constructor and data constructors.
Data constructors are the values of a particular type; they are also functions that let us create data, or values, of a particular type. although it will take some time before the full import of this becomes clear.
In Haskell, you cannot create untyped data, so except for a sprinkling of syntastic sugar for things like numbers or functions, everything originates in a data construtor from some definition of a type.

In this chapter we are going to look at:

* querying and reading type signatures
* see what _cyrrying_ is
* take a closer look at _polymorphism_
* type inference and how to declare types for our functions

## 5.2 What are types for?

_Types_ are kind of taken from type systems in logic and mathematics, and have been designed to impose constraints that
enforce correctness. A type system catches errors, so you for example can't add a number to a String and expect a valid result.
If a function expects to receive a Bool for example, and you give it a list, you will get a type error, thus saving you from errors for free.

## 5.3 How to read type signatures
### Understanding the function type

The arrow `(->)` is the type constructor for functions in Haskell. So it's a constructor type like `Bool` except the _type constructor_ takes arguments and has no _data constructors_.

```haskell
Prelude> :i (->)
data (->) a b
```

The next bit you should just read in the book.

### Typeclass-constrained type variables

If we look at the following:

```haskell
Prelude> :t (+)
(+) :: Num a => a -> a -> a
Prelude> :t (/)
(/) :: Fractional a => a -> a -> a
```

For `(+)` you can casually say that you need a numberic value (the constraint Num a), another numeric value and then it returns a numeric value, all of the same type, as if you give it one `a`, all the other `a`'s has to be the same.
For `(/)` the same applies, though here we are constrained by the fact that we need a value that implements the typeclass _Fractional_.

For a newbie right now, it's kind of hard to explain types, but the compiler will always give you the least specific type. And when you are constrained by a typeclass, it is because we don't yet know the concrete type of `a`.

_Num_ being the most generel typeclass, and then afterwards we could be more specific and call our a an _Int_ for example. Int will have an instance of the Num typeclass. A typeclass can also be seen as a class where that implements a number of standard functions. So Int, implementing Num, will have access to all the the functions of Num, like plus, minus, etc.

## 5.4 Currying

In other languages you can usally construct a function that takes multiple arguments. In Haskell, it might seem like you can too, but really there are syntactic conveniences that construct _curried_ functions. A curried function is a function that takes and argument, and with that argument returns another function that then takes another argument.
So when we look at a function like `f :: a -> a -> a` it might look like it takes two arguments, but really what happens is this
`f :: a -> (a -> a)`, because the `(->)` operator is right associative.
So the first _arrow_ takes an argument and then returns another function (the arrow inside the parentheses).

### Partial application

Partial application is related to currying and is when you apply a function to to less arguments than needed. So if a function takes two arguments, you can apply one argument to it, and save that application for later, and apply the last argument.
Example:
```haskell
addStuff :: Integer -> Integer -> Integer
addStuff a b = a + b

Prelude> addFive = addStuff 5
Prelude> :t addFive
addFive :: Num a => a -> a
Prelude> addFive 10
15
```

## 5.4 Currying

All functions in Haskell accept one argument and return a result. Passing
multiple arguments through is syntactic sugar for what's really going on.

`(->)` is infix and right associative:

```haskell
f :: a -> a -> a
-- associates to
f :: a -> (a -> a)

map :: (a -> b) -> [a] -> [b]
-- associates to
map :: (a -> b) -> ([a] -> [b])
```

This grouping serves to group parameters into argument and result - it does not
change the order of evaluation.


### Partial application

```haskell
  subStuff :: Integer -> Integer -> Integer
  subStuff a b = a - b - 10

  -- which is the same as
  subStuff :: Integer -> (Integer -> Integer)

  -- if we attempt the following
  subOne = subStuff 1
  -- we won't be subtracting 1 from anything, because of how arguments are partially
  -- applied in Haskell:
  -- subOne b = 1 - b - 10
  -- Arguments are applied left to right
```

## Manual currying and uncurrying

To uncurry `(+)` we need to pass in both arguments at once. This can be done using
a tuple.

```haskell
  -- Initially we have the following for (+)
  (+) :: Num a => a -> a -> a

  -- Uncurried
  (+) :: Num a => (a, a) -> a
```

- _Uncurried_ functions - one function, many arguments
- _Curried_ functions - many functions, one argument each

The following 4 functions are equivalent:

```haskell
  -- setup
  nonsense :: Bool -> Integer
  nonsense True = 805
  nonsense False = 31337

  -- curried
  curriedFunction :: Integer -> Bool -> Integer
  curriedFunction i b = i + (nonsense b)

  -- uncurried using tuple
  uncurriedFunction :: (Integer, Bool) -> Integer
  uncurriedFunction (i, b) = i + (nonsense b)

  -- curried using a lambda
  anonymous :: Integer -> Bool -> Integer
  anonymous = \i b -> i + (nonsense b)

  -- manually currying with multiple lambdas
  -- this function doesn't use Haskell's autocurrying
  anonNested :: Integer -> Bool -> Integer
  anonNested = \i -> \b -> i + (nonsense b)
```

### Currying and uncurrying existing functions

We can curry existing functions by considering how uncurried functions already
work.

Uncurried functions take a tuple (basically multiple arguments). If we take the
original uncurried function, and then manually build the tuple, we can create a
curried version of the function:

```haskell
  -- take a function, and 2 constituents we build into a tuple, and apply f to
  -- the tuple
  curry :: f -> a -> b -> f (a, b)

  -- we know that fst has the following type signature:
  -- fst :: (a, b) -> a

  -- we can now build a curried version of fst
  curriedFst :: a -> b -> a
  curriedFst a b = curry fst a b

  curriedFst 2 1
  2
```

We can uncurry a function similarly:

```haskell
  -- take a function and 2 arguments in a tuple, and then apply the function to
  -- them at once
  uncurry :: f -> (a, b) -> f a b
  uncurry f (a, b) = f a b

  -- we can now uncurry (+)
  uncurriedAddition :: Num a => (a, a) -> a
  uncurriedAddition (a, b) => uncurried (+) (a, b)

  uncurriedAddition (1, 2)
  3
```

### Sectioning

_Sectioning_ refers to partial application of infix operators.

_Sectioning_ allows one to specify whether an infix operator should be partially
applied to the first or second argument:

```haskell
  baseTwo :: Num a -> a
  baseTwo = (2^)

  square :: Num a -> a
  square = (^2)

  baseTwo 3
  8

  square 3
  9
```

This can also be done with prefix functions that have a backtick infix
equivalent:

```haskell
  intDivByThree :: Num a => a
  intDivByThree = `div` 3

  intDivOfThree :: Num a => a
  intDivOfThree = 3 `div`
```

### Exercises: Type arguments

1.
    ```haskell
      -- given
      f :: a -> a -> a -> a

      -- and x :: Char
      -- then
      f x :: Char -> Char -> Char
    ```

2.
    ```haskell
      -- given
      g :: a -> b -> c -> b

      -- then
      g 0 'c' "woot" :: Char
    ```

3.
    ```haskell
      -- given
      h :: (Num a, Num b) => a -> b -> b

      -- then
      h 1.0 2 :: Num b => b
    ```

4.
    ```haskell
      -- given
      h :: (Num a, Num b) => a -> b -> b

      -- then
      h 1 (5.5 :: Double) :: Double
    ```

5.
    ```haskell
      -- given
      jackal :: (Ord a, Eq b) => a -> b -> a

      -- then
      jackal "keyboard" "has the word keyboard" :: [Char]
    ```

6.
    ```haskell
      -- given
      jackal :: (Ord a, Eq b) => a -> b -> a

      -- then
      jackal "keyboard" :: Eq b => b -> [Char]
    ```

7.
    ```haskell
      -- given
      kessel :: (Ord a, Num b) => a -> b -> a

      -- then
      kessel 1 2 :: (Ord a, Num a) => a

      -- Because 'b' has a Num constraint 'a' requires the constraint, too, in
      -- order for whatever Ord is required for to operate on both values.

      -- If we evaluate Ord using (>) we can see what happens as arguments are
      -- passed in:
      :t (>)
      (>) :: Ord a => a -> a -> Bool

      -- the partially applied (>) expects the next parameter with an
      -- additional constraint
      :t (>) 1
      (>) 1 :: (Ord a, Num a) => a -> Bool

      :t (>) 'a'
      (>) 'a' :: Char -> Bool
    ```

8.
    ```haskell
      -- given
      kessel :: (Ord a, Num b) => a -> b -> a

      -- then
      kessel 1 (2 :: Integer) :: (Ord a, Num a) => a

      -- this is the same as in 7
      -- we don't care about the type of 'b', because we don't use it. We care
      -- only about the constraints imposed on 'a' as a result of the signature
    ```

9.
    ```haskell
      -- given
      kessel :: (Ord a, Num b) => a -> b -> a

      -- then
      kessel (1 :: Integer) 2 :: Integer

      -- Integer is the type because it (mostly) doesn't matter what type 'b' is;
      -- kessel only returns the value of 'a', therefore our type is that of 'a',
      -- which in this case is Integer
    ```

## 5.5 Polymorphism

_Polymorphic_ means "made of many forms", which is in contrast with
_monomorphic_; made of one form.

Type signatures may have three types:

- concrete types
- constrained polymorphic types
- parametric polymorphic types

_Constrained polymorphism_ in Haskell is sometimes referred to as _ad-hoc_
polymorphism in other langauges.

_Ad-hoc polymorphism_ is implemented with typeclasses in Haskell.

_Paramatric polymorphism_ refers to type variables, parameters, that are fully
polymorphic; the final concrete type of a variable could be anything.

If a variable can be anything, then there's not much that can be done with it.
If a variable has a type, then it has access to the methods available to it in
that type.

Types inherit from their superclasses, and may not override their superclass
methods.

```haskell
  -- Num is a superclass of Int
  :i Num
    instance Num Int

  -- Integral is a superclass of Int
  :i Integral
    instance Integral Int

  -- or more succinctly
  :i Int
    instance Integral Int
    instance Num Int

  -- Int inherits methods from both Integral and Num
```

### Exercises: Parametricity

1.
    ```haskell
      -- given
      f :: a -> a

      -- there's nothing we can do apart from define f as follows:
      f a = a

      -- we have to return just 'a', because we have no idea what methods are
      -- available to 'a' because it is parametrically polymorphic.
    ```

2.
    ```haskell
      -- given
      f :: a -> a -> a

      -- we can only define 2 functions, because we have 2 parameters, but know
      -- nothing about which methods are available to apply to them:
      f a b = a

      -- or
      f a b = b
    ```

3.
    ```haskell
    -- given
    f :: a -> b -> b

    -- we can only implement this by throwing the first argument away, because there a no constraints.
    -- and for it to work, we have to do the most general thing, because we could not for example add a string and a number
    f a b = b
    ```
### Polymorphic constraints

Numeric values are by default polymorphic until assigned a more specific type.

```haskell
:t (-10)
(-10) :: Num a => a
```

This is called a _polymorphic constant_.

We can assign more specific typeclasses to make values concrete:

```haskell
x = 5 :: Integer

:t x
x :: Integer
```

## 5.6 Type inference

Haskell does not enforce type signatures because it uses type inference to
determine the type of an expression.

The compiler infers the most polymorphic type that is still correct.

e.g. Numeric values are assigned `Num` unless they need to be more specific,
such as in the case when using `(/)`.

### Exercises: Apply Yourself

1.
    ```haskell
    -- given
    (++) :: [a] -> [a] -> [a]

    -- how will it change when we apply it in the following way
    myConcat x = x ++ "yo"

    -- we get
    myConcat :: [Char] -> [Char]
    ```

2.
    ```haskell
    -- given
    (*) :: Num a => a -> a -> a

    -- if we apply it
    myMult x = (x / 3) * 5

    -- we get
    myMult :: Fractional a => a -> a
    ```

3.
   ```haskell
   -- given
   take :: Int -> [a] -> [a]

   -- applied
   myTake x = take x "hey you"

   -- we get
   myTake :: Int -> [Char]
   ```

4.
   ```haskell
   -- given
   (>) :: Ord a => a -> a -> Bool

   -- applied
   myCom x = x > (length [1..10])

   -- we get
   myCom :: Int -> Bool
   ```

5.
   ```haskell
   -- given
   (<) :: Ord a => a -> a -> Bool

   -- applied
   myAlph x = x < 'z'

   -- we get
   myAlph :: Char -> Bool
   ```

# Asserting types for declarations

Prefer declaring types over relying on inference. It makes for better error
messages, and makes for code that is easier to understand down the line, and for
other people to understand.

## 5.8 Exercises

### Multiple Choice

1. A value of type `[a]` is
    c.) a list whose elements are all of some type `a`
2. A function of type `[[a]] -> [a]` could
    a.) take a list of Strings as an argument
3. A function of type `[a] -> Int -> a`
    b.) returns one element of type `a` from a list
4. A function of type `(a, b) -> a`
    c.) takes a tuple argument and returns the first type

### Determine the type

1. 
    a. Where
    ```haskell
    a = (*9) 6
    ```

    ```haskell
    a :: Num a => a
    ```

    b. Where
    ```haskell
    b = head [(0, "doge"), (1, "kitteh")]
    ```

    ```haskell
    b :: Num a => (a, [Char])
    ```

    c. Where
    ```haskell
    c = head [(0 :: Integer, "doge"), (1, "kitteh")]
    ```

    ```haskell
    c :: (Integer, [Char])
    ```

    d. Where
    ```haskell
    d = if False then True else False
    ```

    ```haskell
    d :: Bool
    ```

    e. Where
    ```haskell
    e = length [1, 2, 3, 4, 5]
    ```

    ```haskell
    e :: Int
    ```

    f. Where
    ```haskell
    f = (length [1, 2, 3, 4]) > (length "TACOCAT")
    ```

    ```haskell
    f :: Bool
    ```

2. 
    ```haskell
    x = 5
    y = x + 5
    w = y * 10
    ```

    ```haskell
    w :: Num a => a
    ```

3. 
    ```haskell
    x = 5
    y = x + 5
    z y = y * 10
    ```

    `z`s `y` is scoped to `z`. The `y` declared outside is ignored, and

    ```haskell
    z :: Num a => a -> a
    ```

4. 
    ```haskell
    x = 5
    y = x + 5
    f = 4 / y
    ```

    ```haskell
    f :: Fractional a => a
    ```

5. 
    ```haskell
    x = "Julie"
    y = " <3 "
    z = "Haskell"
    f = x ++ y ++ z
    ```

    ```haskell
    f :: [Char]
    ```

### Does it compile?

1. Given
    ```haskell
    bigNum = (^) 5 $ 10
    wahoo = bigNum $ 10
    ```

    `wahoo` does not resolve. `($)`s type signature shows that as an infix operator, it
    expects the first argument to be a function, and the second some other term
    to apply to that function.

    i.e.
    ```haskell
    :t ($)
    ($) :: (a -> b) -> a -> b
    ```

    To resolve, we need the expression before `$` in `wahoo` to be a function:

    ```haskell
    wahoo = (+) bigNum $ 10
    ```

    `$` is redudant in both functions here, because all it is doing is applying
    the function on its left to the value on its right - something the functions
    are doing anyways.


2. Given
    ```haskell
    x = print
    y = print "woohoo! "
    z = x "hello world"
    ```

    Does compile. This is point-free style - x takes no explicit arguments.

3. Given
    ```haskell
    a = (+)
    b = 5
    c = b 10
    d = c 200
    ```

    Does not compile, because in `c` `b` can't be applied to `10` because `b` is
    already in normal form.

    To resolve one could add some operator to `c`:

    ```haskell
    c = a b 10
    ```

    Now `d` will not compile, because `c` is also in normal form. We need a
    function to apply the two values in `d` to:

    ```haskell
    d = a c 200
    ```

4. Given
    ```haskell
    a = 12 + b
    b = 10000 * c
    ```

    Does not compile. `b` is used before it is in scope (this only matters in
    GHCi), and `c` is not in scope at all.

    T resolve:
    ```haskel
    c = 1
    b = 1000 * c
    a = 12 + b
    ```

### Type variable or specific type constructor?

Variables can be one of three types:

- Fully polymorphic type variables - not constrained to any type e.g.
    ```haskell
    id a = a

    -- id :: a -> a
    -- where
    -- a :: *
    ```
- Constrained polymormphic type variables - constrained to a type
    ```haskell
    add a b = (+) a b
    -- add :: Num a => a -> a -> a
    -- where both a and b
    -- t :: Num
    ```
- Concrete type variables
    ```haskell
    -- strLength :: [Char] a => a -> Int
    -- where a has to be of type [Char]
    ```

1. Where
    ```haskell
    f :: Num a => a -> b -> Int -> Int
    --           [0]  [1]   [2]    [3]
    -- [0] - constrained polymorphic
    -- [1] - fully polymorphic
    -- [2] [3] - concrete
    ```

2. Where
    ```haskell
    f :: zed -> Zed -> Blah
    --   [0]    [1]     [2]
    -- [0] - fully polymorphic
    -- [1] [2] - concrete
    ```

3. Where
    ```haskell
    f :: Enum b => a -> b -> C
    --            [0]  [1]  [2]
    -- [0] - fully polymorphic
    -- [1] - constrained polymorphic
    -- [2] - concrete
    ```

4. Where
    ```haskell
    f :: f -> g -> C
    --  [0]  [1]  [2]
    -- [0] - fully polymorphic
    -- [1] - fully polymorphic
    -- [2] - concrete
    ```

### Write a type signature

1. Given
    ```haskell
    -- given
    functionH (x:_) = x

    -- then
    functionH :: [a] -> a
    ```

2. Given
    ```haskell
    -- given
    functionC x y = if (x > y) then True else False

    -- then
    functionC :: Ord a => a -> a -> Bool
    ```

3. Given
    ```haskell
    -- given
    functionS (x, y) = y

    -- then
    functionS :: (a, b) -> b
    ```

### Given a type, write the function

1. Given
    ```haskell
    i :: a -> a
    i x = x
    ```

2. Given
    ```haskell
    c :: a -> b -> a
    c x y = x
    ```

3. Given
    ```haskell
    c'' :: b -> a -> b
    ```
    `c''` is equivalent to `c`

4. Given
    ```haskell
    c' :: a -> b -> b
    c x y = y
    ```

5. Given
    ```haskell
    r :: [a] -> [a]
    r x = x
    -- or
    r x = f x
        where f :: [a] -> [a]
        -- f could be take, tail, drop, etc.
    ```

6. Given
    ```haskell
    co :: (b -> c) -> (a -> b) -> a -> c
    co bToC aToB a = bToC (aToB a)
    ```

7. Given
    ```haskell
    a :: (a -> c) -> a -> a
    a _ x = x
    ```

8. Given
    ```haskell
    a' :: (a -> b) -> a -> b
    a' xToY x = xToY x
    ```

### Fix it

1. [fix-it-1-sing.hs](./fix-it-1-sing.hs)
2. [fix-it-2-sing.hs](./fix-it-2-sing.hs)
3. [fix-it-3-arith3broken.hs](./fix-it-3-arith3broken.hs)

### Type-Kwon-Do

1.
    ```haskell
      -- given
      f :: Int -> String
      f = undefined

      g :: String -> Char
      g = undefined

      h :: Int -> Char

      -- then
      h a = g (f a)
    ```

2.
    ```haskell
      -- given
      data A
      data B
      data C

      q :: A -> B
      q = undefined

      w :: B -> C
      w = undefined

      e :: A -> C

      -- then
      e a = w (q a)
    ```

3.
    ```haskell
      -- given
      data X
      data Y
      data Z

      xz :: X -> Z
      xz = undefined

      yz :: Y -> Z
      yz = undefined

      xform :: (X, Y) -> (Z, Z)

      -- then
      xForm (a, b) = (xz a, yz b)
    ```

4.
    ```haskell
      munge :: (x -> y) -> (y -> (w, z)) -> x -> w
      munge xToY yToTuple x = fst (yToTuple ( xToY x ))
    ```

## 5.9 Definitions

_Polymorphism_ refers to type variables which may have more than one concrete
type.

_Principal type_ is the most generic type that still typechecks (e.g. `Num` for
numeric values is often the _principal type_ when a type is inferred).

A _type variable_ is a way to refer to an unspecified type or set of types in
Haskell type signatures.

_Ad-hoc_ or _constrained_ polymorphism is polymorphism that applies one or more
typeclass constraints to what would have been a parametrically polymorphic type
variable.

```haskell
-- ad-hoc polymorphism via the Num typeclass
(+) :: Num a => a -> a -> a

-- vs parametric polymorphism
c' :: a -> a -> a
```


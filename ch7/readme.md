# Chapter 7 - More functional patterns

## 7.1 Make it func-y

In this chapter we are going to look at and demonstrate that:

* Haskell functions are first-class entities that
* can be values in expressions, lists or tuples;
* can be passed as arguments to a function;
* can be returned from a function as a result;
* make use of syntactic patterns

## 7.2 Arguments and parameters

All Haskell values can be arguments to functions. A value like that is called a _first-class_ value.
Functions can be arguments to other functions in Haskell.

### Setting parameters

First we'll define a value with no parameters:

```haskell
myNum :: Integer
myNum = 1

myVal = myNum

-- if we quere the type of myVal

Prelude> :t myVal
myVal :: Integer
```

`myVal` has the same type as `myNum` because it is equal to it.
Now we introduce a parameter named f:

```haskell
myNum :: Integer
myNum = 1

myVal f = myNum

-- if we quere the type of myVal

Prelude> :t myVal
myVal :: t -> Integer
```

After we parameterized `myVal`, the type changes. The type `t` is polymorphic, because we don't do anything with it - it
could be anything! It inferred the maximally polymorphic type. If we do something with the `f` it will change the type:

```haskell
myNum :: Integer
myNum = 1

myVal f = f + myNum

-- if we quere the type of myVal

Prelude> :t myVal
myVal :: Integer -> Integer
```

Now it knows that `f` has to be of type _Integer_ because we add it to `myNum`.

If we add more parameters, to the original function without adding to `myNum`, the same thing happens as before:

```haskell
myNum :: Integer
myNum = 1

myVal f g = myNum

-- if we quere the type of myVal

Prelude> :t myVal
myVal :: t -> t1 -> Integer
```

Again, they infer the maximally polymorphic type, because we don't do anything with them.

### Binding varriables to values

Let's consider how the binding of variables works. Applying a function binds its parameters to values.
Type parameters become bound to a type, and a function variables are bound to a value.
The binding of values concerns not only the application of function arguments, but also things like `let` expressions and `where` clauses.

```haskell
addOne :: Integer -> Integer
addOne x = x + 1
```

When `addOne` is applied to a value we say that _x_ is now _bound_ to the value the function was applied to. Until a function's arguments have been applied, we cannot make use of the result of the function.

We can use `let` expressions to declare and bind variables as well:

```haskell
bindExp :: Integer -> String
bindExp x =
  let y = 5 in
  "the integer was: " ++ show x
  ++ " and y was: " ++ show y
```

In `show y` the `y` is only in scope _inside_ the `let` expression. This is _lexical scoping_ that means a variable declared in the innermost code, will be used, and will not be visible to any expression outside of `let` and `where` clauses.

## 7.3 Anonymous functions

An anonymous function is a function "_without a name"_. We usually use them, if we only need to use them once, and usually as a first-class function, as an argument to another function.

```haskell
triple :: Integer -> Integer
triple x = x * 3

-- can be written with anonymous syntax:

(\x -> x * 3) :: Integer -> Integer
```

We wrap it in parentheses to be able to apply arguments to the function, like `(\x -> x * 3) 5`.

### Exercises: Grab Bag

1. Which (two or more) of the following are equivalent?
   ```haskell
   -- a)
   mTh x y z = x * y * z
   -- b)
   mTh x y = \z -> x * y * z
   -- c)
   mTh x = \y -> z -> x * y * z
   -- d)
   mTh = \x -> \y -> \z -> x * y z
   ```
   They are all equivalent.

2. The type of the above function is `Num a => a -> a -> a -> a`
   Which is the type of `mTh 3`?

   The type is `Num a => a -> a -> a`

3. Rewrite the following:

   a.)

   ```haskell
   -- given
   addOneIfOdd n = case odd n of
    True -> f n
    False -> n
    where f n = n + 1

   -- into
   addOneIfOdd n = case off n of
    True -> f n
    False -> n
    where f = \n -> n + 1
   ```

   b.)

   ```haskell
   -- given
   addFive x y = (if x > y then y else x) + 5

   -- into
   addFive = \x -> \y -> (if x > y then y else x) + 5
   ```

   c.)

   ```haskell
   -- given
   mFlip f = \x -> \y -> f y x

   -- into
   mFlip f x y = f y x
   ```

## 7.4 Pattern matching

Pattern matching is _awesome_!
Pattern matching matches against values or data constructors, _not_ types. Matching a pattern may fail, proceeding to the next available pattern to match or succeed. When a match succeeds, the variables exposed in the pattern are bound.
Pattern matching proceedes from let to right and from outside to inside.
We can pattern match on numbers, in the following example, if the _Integer_ argument to the function equals _2_, this will return `True`, otherwise `False`.

```haskell
isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False
```

Note the `_` after the match against the value `2`. This is a means of defining a universal pattern that never fails to match. If you give it _anything_ else than `2`, the function returns `False`. When you pattern match you should order your cases from most specific to least specific.

### Pattern matching against data constructors

Again, pattern matching is awesome. We can pattern match against data conscructors, like this [this example](../ch7/registeredUser1.hs)

Notice how the type of `RegisteredUser` is a function that constructs a `User` out of two arguments, `Username` and `AccountNumber`. This is what we mean when we refer to a value as a "data constructor".

Notice in the example how we are unpacking the values of `RegisteredUser` via pattern matching. The idea of unpacking is important, so let's examine [another example.](../ch7/wherePenguinsLive.hs)

In this example we pattern match again, not much to say.

### Pattern matching on tuples

Earlier in the chapter, we had a function like this:

```haskell
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))
```

We can write that in a little more intuitive way, using pattern matching:

```haskell
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (b, c) = ((b, d), (a, c))
```

The function definition looks a lot more like the type.

And now a bit more examples of tuple functions:

```haskell
module TupleFunctions where

-- using pattern matching
addEmUp :: Num a => (a, a) -> a
addEmUp (x, y) = x + y

-- using the normal inbuilt functions
addEmUpAlt :: Num a => (a, a) -> a
addEmUpAlt x = (fst x) + (snd x)

-- again using pattern matching
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, x) = x
```

### Exercises: Variety pack

1.
    ```haskell
    -- Given the following:
    k (x, y) = x
    k1 = k ((4-1), 10)
    k2 = k ("three", (1 + 2))
    k3 = k (3, True)
    ```
    a) What kind of type does `k` have?
    The type of `k` is `k :: (a, b) -> a`

    b) What is the type of `k2`? Is it the same as `k1` or `k3`?
    The type of `k2` is `k2 :: String`, and not the same as the other two

    c) Of `k1`, `k2` and `k3`, which will return the result of 3?
    `k1` and `k3` will return 3.

2.
    Fill in the definition of the following function:
    ```haskell
    f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f)
    f (a, _, c) (d, _, f) = ((a, d), (c, f))
    ```

## 7.5 Case expressions

_Case expressions_ are kind of similar to _if-then-else_ expressions, in the way we can make a function return different results, depending of different outputs.
You can use case expressions with any datatype that has visible data constructors. Consider `Bool`.

```haskell
data Bool = False | True
```

As with almost everything else, we need to consider both cases, `True` and `False`. Let's looke at a simple `in then else` expressions from earlier chapters.

```haskell
:: (Eq a, Num a) => a -> String
if x + 1 == 1 then "AWESOME!" else "wut"
```

We can rewrite this expression using a _case expression_, matching on the data constructors of Bool.

```haskell
funcZ :: (Eq a, Num a) => a -> String
funcZ x =
  case x + 1 == 1 of
    True -> "AWESOME"
    False -> "wut"
```

We can also rewrite a different earlier function, where we checked if a word is a palindrome.

```haskell
pal :: Eq a => [a] -> String
pal x =
  case x == reverse x of
    True -> "yes"
    False -> "no"

-- you can also write it with a where clause

pal' :: Eq => [a] -> String
pal' x =
  case y of
    True -> "yes"
    False -> "no"
  where y = x == reverse x
```

We can also rewrite another function from earlier, where we had an especially cool or cold greeting, depending what you gave it.

```haskell
module GreetIfCool3 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
  case cool of
    True -> putStrLn "eyyy, what's shakin'?"
    False -> putStrLn "psssh"
  where cool = coolness == "downright frosty yo"
```

### Exercises: Case practice

We need to rewrite a couple of _if then else_ expresssions.

1.
    The following should return `x` when `x` is greater than `y`.
    ```haskell
    functionC :: Ord a => a -> a -> a
    functionC x y = if (x > y) then x else y

    -- with a case expression
    functionC' :: Ord a => a -> a -> a
    functionC' x y = case x > y of
      True -> x
      False -> y
    ```

2.
    This function will add 2 to even numbers else return the input number
    ```haskell
    ifEvenAdd2 :: Integral a => a -> a
    ifEvenAdd2 n = if even n then (n + 2) else n

    -- with a case expression
    ifEvenAdd2' :: Integral a => a -> a
    ifEvenAdd2' n = case even n of
      True -> n + 2
      False -> n
    ```
3.
    The next function does not have all cases covered, fix it.
    ```haskell
    nums :: (Ord a, Num a, Num b) => a -> b
    nums x = case compare x 0 of
      GT -> 1
      LT -> -1

    -- fixed:
    nums :: (Ord a, Num a, Num b) => a -> b
    nums x = case compare x 0 of
      GT -> 1
      EQ -> 0
      LT -> -1
    ```

## 7.6 Higher-order Functions

_Higher-order functions_ are functions that take other functions as it's argument. Very important in functional programming, as it lets combine functions efficiently.

`flip` is a higher-order function (HOF) that flips the arguments to a two argument function. An simple example could be.

```haskell
flip :: (a -> b -> c) - b -> a -> c)
flip f x y = f y x

-- (-) works as you would expect
Prelude> (-) 10 9
1

-- But if we supply flip with the (-) function, thereby flipping the arguments, we get
Prelude> flip (-) 10 9
-1

-- So we flipped the arguments, and thereby instead of 10-9 we got 9-10!
```

Let's write an example where we also use data structures.

```haskell
module Coders where

data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Show, Eq, Ord)

-- lets build a function that should really not be used on it's own, beacuse I can say that a Coder is a boss of a Veep for example, but we need it.
reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ show e ++ " is the boss of " ++ show e'

-- now lets build the function that needs the before created function.
employeeRank :: Employee -> Employee -> IO ()
employeeRank e e' = case compare e e' of
                      GT -> reportBoss e e'
                      EQ -> putStrLn "These employees have the same rank"
                      LT -> (flip reportBoss) e e'

-- now the coders are obviously the backbone of a company and they have access to the code!
changedEmployeeRank :: (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO ()
changedEmployeeRank f e e' = case f e e' of
                               GT -> reportBoss e e'
                               EQ -> putStrLn "These employees have the same rank"
                               LT -> (flip reportBoss) e e'

-- and now that we have a changedEmployeeRank that accepts another function as it's argument, we have a HOF, and we can exploit it!
codersRule :: Employee -> Employee -> Ordering
codersRule Coder Coder = EQ
codersRule Coder _     = GT
codersRule _     Coder = LT
codersRule e e'        = compare e e''
```


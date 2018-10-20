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

## 7.7 Guards

Guards are kind of similar to _if-then-else_ expressions, in the way that they rely on boolean evaluation to decide between two _or more_ results.

### Writing guards blocks

Here's an example:

```haskell
-- written with if-then-else
myAbs :: Integer -> Integer
myAbs x = if x < 0 then (-x) else x

-- written with guard notation
myAbs' :: Integer -> Integer
myAbs' x
  | x < 0     = (-x)
  | otherwise = x
```

We use a `|` to begin a _guard case_, and the value before the `=` must always evaluate to `Bool`.
`otherwise` is another word for `True`, and is used as a fallback case, because we must consider all the options, like pattern matching.

Image we had a program for evaluating levels of some component in blood, we could write a function to tell us
if everything is as it should be (simplfied).

```haskell
bloodLevels :: Integer -> String
bloodLevels x 
  | x > 145   = show x ++ " is too high!"
  | x < 135   = show x ++ " is too low!"
  | otherwise = show x ++ " is just right."
```

The most useful function in the world would be how to calculate how old your dog is in human years!

```haskell
dogYears :: Integer -> Integer
dogYears x
  | x <= 0     = 0
  | x <= 1     = x * 15
  | x <= 2     = x * 12
  | x <= 3     = x * 8
  | otherwise = x * 6
```

We can also use _where_ clauses in guard blocks. Let's say we have a test with 100 questions and we want to calculate the grade from
the number of correct answers:

```haskell
averageGrade :: (Fractional a, Ord a) => a -> Char
averageGrade x
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  | y < 0.59  = 'F'
  where y = x / 100
```

### Exercises: Guard Duty

1.
    If you put the _otherwise_ clause as your top most guard, what would happen?  
    A: We would always evaulate that guard clause, as _otherwise_ is the same as saying _True_.

2.
    What happens if you take `averageGrade` and reorders the guards? What happens if you move `| y >= 0.7 = 'C'` to the top?  
    A: Again anytime this guard clause would be evaluated to _True_, we would return _C_, and that means you could never get an _A_ or _B_ anymore.

3.
    The following function returns:  
    ```haskell
    pal xs
      | xs == reverse xs = True
      | otherwise        = False
    ```
    b) True when `xs` is a palindrome.

4.
    What type of arguments can `pal` take?  
    A: List of a's or `[a]`.

5.
    What is the type of the function `pal`:  
    `pal :: Eq a => [a] -> Bool`

6.
    The following function returns:  
    ```haskell
    numbers x
      | x < 0  = -1
      | x == 0 = 0
      | x > 0  = 1
    ```
    c) An indication of whether it's argument is a positive or negative number, or zero.

7.
    What types of arguments can `numbers` take?  
    A: Any number that can be ordered, so `(Ord a, Num a) => a`.

8.
    What is the type of the function `numbers`?  
    A: `(Ord a, Num a, Num b) => a -> b`

## 7.8 Function composition

Function composition is a type of higher-order function that allows us to combine functions such that the result of
applying one function gets passed to the next function as an argument. Function composition is basically in the 
spirit of Haskell, being based so much on lambda calculus.
Let us look at the type signature for `(.)` which is function composition.

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
--      [1]          [2]      [3]  [4]
```

1. is a function from b to c.
2. is a function from a to b.
3. is a value of type a, the same as [2] expects.
4. is a value of type c, the same as the output of [1]

So in plain english, given a function from _b_ to _c_, a function from _a_ to _b_, return a function from _a_ to _c_.

`a -----> b ----> c`

So if you compose to functions in Haskell, you get something like this:

```haskell
(f . g) x = f (g x)
```
If we take a specific example, we can negate the sum of a list, like so:

```haskell
negate . sum $ [1..5]
```

We still need the `($)` operator, if we didn't the expression would evaluate like so:

```haskell
negate . sum [1..5]
-- to
negate . 15
```

And you cannot compose a function with a numeric value. This happens because function application is the highest precedence, whitespace (10 out of 10), so the expression
will evalute `sum` first, and then try to compose afer. We need the `($)` to tell our compiler to wait with function application, to _after_ we have composed
the two functions.

We could also write the function as follows:

```haskell
(negate . sum) [1..5]
-- or
negate (sum [1..5])
```

So you may be wondering why even bother with function composition, when all it seems to do is remove the need for nesting functions in parentheses.
The reason is that function composition makes in incredibly easy to compose more than _two_ functions, example:

```haskell
take5Odds :: Integral a => a -> [a]
take5Odds x = take 5 . filter odd . enumFrom $ x

Prelude> take5Odds 10
[11,13,15,17,19]
```

This function is a composition of three functions. First we enumerate from x, 10 in this case, which will create a list, starting from 10.
Then we pipe that data into the higher-order function `filter`, where the condition is to filter out all the odd numbers, and return that to a list.
And in the end we take 5 elements of that list. See, easy to compose more than two functioons.

## 7.9 Pointfree style

This section I won't cover, because I really don't like pointfree style. It's a style of composing functions without specifying their arguments, and I feel
that it makes the programs harder to reason about. That is just my opinion, so you are of course free to go read the section.

## 7.10 Demonstrating composition

The two functions `putStr` and `print` were mentioned earlier. They seem similar on the surface, but behave differently because
of the underlying types.

```haskell
putStr :: String -> IO ()
putStrLn :: String -> IO ()
print :: Show a => a -> IO ()
```

They both return a result of `IO ()` for reasons I think we discussed earlier in the chapter, but they take different
arguments. One takes a `String` and the other has a constrained polymorphic parameter, `Show a => a`.
You may also recall a function from earlier called `show`.

```haskell
show :: Show a => a -> String
```

So maybe you have already figured it out, but fortunately it was understood that combining `putStrLn` and `show` would be
a common pattern, so the function named `print` is a composition the former two functions.

```haskell
print :: Show a => a -> IO ()
print a = putStrLn (show a)
-- we can express this using function compusition as well
print :: Show a => a -> IO ()
print a = (putStrLn . show) a
```

As you might have noticed here, `show` takes an argument of `Show a => a` and returns a `String`. What kind of argument does `putStrnLn` expect? A `String`!

## 7.11 Chapter Exercises

### Multiple choice

1.
    A polymorphic function...  
    d) may resolve to values of different types, depending on inputs

2.
    Two functions named `f` and `g` have types `Char -> String` and `String -> [String]` respectively. The composed function
    `g . f` has the type?  
    b) `Char -> [String]`

3.
    A function `f` has the type `Ord a => a -> a -> Bool` and we apply it to one numeric value. What is the type now?  
    d) `(Ord a, Num a) => a -> Bool`

4.
    A function with the type `(a -> b) -> c`  
    b) is a higher-order function

5.
    Given the following definition of `f`, what is the type of `f True`?  
    ```haskell
    f :: a -> a
    f x = x
    ```
    a) `f True :: Bool`

### Let's write code

1.
    The following function returns the tens digit of an integral argument.
    ```haskell
    tensDigit :: Integral a => a -> a
    tensDigit x = d
      where xLast = x `div` 10
            d     = xLast `mod` 10
    ```
    
    a) First, rewrite it using `divMod`.
    ```haskell
    tensDigit :: Integral a => a -> a
    tensDigit x = d
      where (xLast, _) = divMod x 10
            (_, d)     = divMod xLast 10
    ```
    b) Does the `divMod` version have the same type as the original version?  
    A: Yes, it does.  

    c) Next, let's change it so that we're getting the hundreds digit instead.  
    ```haskell
    hunsDigit :: Integral a => a -> a
    hunsDigit x = d
      where (xLast, _) = divMod x 100
            (_, d)     = divMod xLast 10
    ```

2.
    Implement the function of the type `a -> a -> Bool -> a` once each using a case expression and once with a guard.  
    ```haskell
    foldBool :: a -> a -> Bool -> a
    foldBool = error "Error: need to implement foldBool!"
    ```  
    Implementation:  
    ```haskell
    -- case expression
    foldBool :: a -> a -> Bool -> a
    foldBool x y bool = case bool of
      True  -> y
      False -> x
    -- guards
    foldBool' :: a -> a -> Bool -> a
    foldBool' x y bool
      | bool == False = x
      | otherwise     = y
    ```

3.
    Fill in the definition. Note that the first argument to our function is _also_ a function which can be applied to values.
    Your second argument is a tuple, which can be used for pattern matching:  
    ```haskell
    g :: (a -> b) -> (a, c) -> (b, c)
    g = undefined

    -- implementation
    g :: (a -> b) -> (a, c) -> (b, c)
    g f (x, y) = (f x, y)
    ```

4.
    For this next exercise, we will be writing _pointfree_ code, ugh.
    Typeclasses are dispatched by type. `Read` is a typeclass like `Show`, but it is the dual or "opposite" of `Show`. In
    general, the `Read` typeclass isn't something you should plan to use a lot, but this exercise is structired to teach you
    something about the interaction between typeclasses and types.
    The function `read` in the `Read` typeclass has the type:  
    ```haskell
    read :: Read a => String -> a
    ```

5.
    We have rewrite the function to pointfree, like so:  
    ```haskell
    roundTrip :: (Show a, Read a) => a -> a
    roundTrip a  = read (show a)

    -- pointfree
    roundTrip :: (Show a, Read a) => a -> a
    roundTrip = read . show
    ```

6.
    When we apply `show` to a value such as `(1 :: Int)` the `a` that implements Show is Int, so GHC will use the Int instance
    of the Show typeclass to "stringify" our Int of 1.
    However `read` expects a `String` argument in order to return an `a`. The `String` argument that is the first argument
    to `read` tells the function nothing about what type the "de-stringified" result should be. In the type signature `roundTrip`
    currently has, it knows because the type variables are the same, so the type that is the input to `show` has to be the
    same type as the output of `read`.  
    
    Your task is now to change the type of `roundTrip` in your file to `(Show a, Read b) => a -> b`, and make the expression
    `print (roundTrip 4)` work.  
    ```haskell
    module Arith4 where

    roundTrip :: (Show a, Read a) => a -> a
    roundTrip  = read . show

    roundTrip' :: (Show a, Read b) => a -> b
    roundTrip' = read . show

    main = do
      print (roundTrip 4)
      print (id 4)
      print ((roundTrip' 4) :: Int)
    ```

## 7.12 Chapter Definitions

1.
    _Binding_ or _bound_ is a common word used to indicate a connection, linkage or association between two objects.
    In Haskell we talk about what a value a variable has, e.g., a parameter variable is _bound_ to an argument value,
    meaning the value is passed into the parameter as an input.

2.
    An _anonymous function_ is a function which is not bound to an identifier and is instead passed as an argument
    to a higher-order function or used to create other function.

3.
    _Currying_ is the process of transforming a function that takes multiple arguments into a series of functions
    which each take one argument and return a function as a result.

4.
    _Pattern matching_ is a syntactic way of deconstructing product and sum types to get their inhabitants.
    With products, pattern matching give you the means for destructuring and exposing the contents of products, binding
    one or more values contained therin to names. With sums, pattern matching lets you descriminate which inhabitant of a sum
    you you mean to handle in that match.  

    ```haskell
    data Identity a = Identity a
      deriving (Eq, Show)

    -- Identity is a unary data constructor. Not a product, only contains one value.
    -- When you pattern match on `Identity` you can unpack and expose the `a`.  

    unpackIdentity :: Identity a -> a
    unpackIdentity (Identity x) = x
    ```  

    With a product we can choose to use, none, one or both of the values in the product type.  

    ```haskell
    data Produt a b =
      Product a b
      deriving (Eq, Show)

    productUnpackOnlyA :: Product a b -> a
    productUnpackOnlyA (Product x _) = x

    productUnpackOnlyB :: Product a b -> b
    productUnpackOnlyB (Product _ y) = y

    productUnpack :: Product a b -> (a, b)
    productUnpack (Product x y) = (x, y)
    ```

    With a sum type we can discriminate by the inhabitants of the sum and choose to do different things based on which
    constructor in the sum they were.  

    ```haskell
    data SumOfThree a b c = FirstPossible a
                          | SecondPossible b
                          | ThirdPossible c
                          deriving (Eq, Show)

    sumToInt :: SumOfThree a b c -> Integer
    sumToInt (FirstPossible _)  = 0
    sumToInt (SecondPossible _) = 1
    sumToInt (ThirdPossible _)  = 2

    -- we can selectively ignore inhabitants of the sum

    sumToInt' :: SumOfThree a b c -> Integer
    sumToInt' (FirstPossible _) = 0
    sumToInt' _                 = 1
    ```

    Pattern matching is about your _data_!

5.
    _Bottom_ is a non-value used to denote that a program cannot return a value or a result.

6.
    _Higher-order functions_ are functions that themselves takes functions as arguments.

7.
    _Composition_ is the application of a function to the result of having applied another function. The composition
    operator `(.)` is a higher-order function that takes the two functions it is composes as arguments and returns a
    function of the composition.

8.
    _Pointfree_ is something I don't want to use.









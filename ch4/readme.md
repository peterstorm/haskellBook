# Chapter 4 - Basic Datatypes

## 4.1 Basic Datatypes

Types are an important part of Haskell. It promotes readability and safety, as it allows us to classify and demystify data, once you understand the types.
In the chapter we will
* Review tpyes we have seen in previous chapters
* learn about datatypes, type constructors and data constructors
* work with predefined datatypes
* learn more about type signatures and a bit about typeclasses

## 4.2 What are types?

Expressions when evaluated, reduce to values. Every value has a type.
If two things are of the same type, they have commonality, and that helps you to reason about the code you write.
Of course if two things do not share a type, they can still have other things in common.

## 4.3 Anatomy of a data declaration

The _type constructor_ is the name of the type and is always capitalized.
The _data constructor_ is the value or values that inhabit the type they are defined in. Example:

```haskell
data Bool = False | True
```

You declare a datatype by using the keyword `data`, then write the type constructor and after the equal sign you write your data constructors.
The `|` means it's a _sum type_ which, or a logical disjuntion: _or_. So a `Bool` is `True` _or_ `False`.
The whole thing is called a _data declaration_.

Datatypes also shows up in _types signatures_, which we've seen before. If we check the type of the function `not`:

```haskell
Prelude> :t not
not :: Bool -> Bool
```

We see that it takes a `Bool` and returns a `Bool` and the type signature references the _type constructor_.

However when we _use_ the `not` funtion we use the _data constructors_ or values.

```haskell
Prelude> not True
False
```

### Exercises: Mood Swing

Given the following datatype answer the following question:

```haskell
data Mood = Blah | Woot deriving Show
```

We will look at _deriving Show_ later, for now it just allows us to print the values of the type to the screen.

1. What is the type constructor or name of this type?  
   Answer: Mood

2. If the function requires a `Mood` value, what are the values you can use?  
   Answer: Blah and Woot

3. We want to write a function that changes the Mood of a person. correct the type signature `changeMood :: Mood -> Woot`  
   Answer: `changeMood :: Mood -> Mood`

4. Now write the function and put it in a source file
```haskell
module ChangeMood where

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah
```
## 4.4 Numeric types

I will be honest, that part is way too long. You will have to read up on it when you need to use different numberic types.

## 4.5 Comparing values

In the Prelude we can compare values using the hopefully wellknown _less than_,  _greater than_ and _equals_.

```haskell
Prelude> x = 5
Prelude> x == 5
True
Prelude> x == 6
False
Prelude> x < 7
True
Prelude> x > 3
True
Prelude> x /= 5
False
```

Note the difference between `=` and `==`. `=` is used to declare values, and `==` is used to check for equality. `/=` checks for _not_ equal to.

Then we have some mention of _type class constraints_ which we will get to later as well. Specifically `Eq a` and `Ord a` which are type classes for things that can be compared and ordered.
If a data type does _not_ implement an instance of the above type classes, you cannot compare or order them.

So our `Mood` data type could not compare or order the two data constructors `Blah` or `Woot`, though you could implement it - more on that later.

## 4.6 Go on and Bool me

Loads of examples using the `Bool` data type. Only new thing is the operators `&&` and `||` which respectively means _and_ and _or_.

### Exercises: Find the Mistakes

Some of these lines of code will not compile, identify and fix.

1. `not True && true`  
   Won't compile, fix: `not True && True` evaluates to `False`.

2. `not (x = 6)`  
   Not sure what this should be fixed as, but won't compile.

3. `(1 * 2) > 5`  
   Compiles to `False`.

4. `[Merry] > [Happy]`  
   Does not compile, missing double quotes. Fix: `["Merry"] > ["Happy"]` evaluates to `True`.

5. `[1, 2, 3] ++ "look at me!"`  
   Won't compile as you can't concatenate lists of two different types.

### Conditionals with if-then-else

Haskell does not have 'if' statements, but it does have _if expressions_, example:

```haskell
Prelude> t = "Truthin'"
Prelude> f = "Falsin'"
Prelude> if True then t else f
"Truthin'"
```

So here `True` of course evaluates to `True`, and we return the first branch of the if expression. Had it evaluated to `False` then we would return `"Falsin'"`.

There's a few examples of an if-then-else expression [here](../ch4/greetIfCool.hs) and [here](../ch4/greetIfCool2.hs).

## 4.7 Tuples

_Tuple_ is a type that allows you to store and pass around multiple (and different) values withing a single value.
They look like this: `(1,2)` or `("hello", "world")`.
The constructor of the tuple is `(,)`, the datatype declaration looks like this:

```haskell
Prelude> :i (,)
data (,) a b = (,) a b
```

A tuple is a _product type_, which is a logical conjunction: you must supply _both_ arguments to construct a value.

We have some convenience functions for the _two-tuple_, like so:

```haskell
fst :: (a, b) -> a
snd :: (a, b) -> b
```

They do what they sound like, return the first or second element of the tuple.

Let's try and implement `fst` and `snd` ourselves, by using _pattern matching_ (more on that later).

```haskell
fst' :: (a, b) -> a
fst' (a, b) = a

snd' :: (a, b) -> b
snd' (a, b) -> b
```

Here is another example of pattern matching with tuples, where we have two different types in a tupple, and have a
function that add them together, using plus for the _Int_ and `++` for the _list_.

```haskell
tupFunc :: (Int, [a]) -> (Int, [a]) -> [Int, [a])
tupFunc (a, b) (c, d) = ((a + c), (b ++ d))
```

## 4.8 Lists

Lists are another type used to contain multiple values withtin a single value. They do however have to be the same type.

## 4.9 Chapter Exercises

```haskell
awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]
```
`length` is a function that takes a list and returns a result of how many items are in the list.

1. Given the definition, what type signature does `length` have?  
   Quess: `[a] -> Int`. Answer: `Foldable t => t a -> Int`

2. What are the results of the following expressions:
* `length [1, 2, 3, 4, 5]`  
  Answer: 5
* `length [(1, 2), (2, 3), (3, 4)]`  
  Answer: 3
* `length allAwesome`  
  Answer: 2
* `length (concat allAwesome)`  
  Answer: 5

3. Given what we know about numeric types and the type signature of `length`, which of these expressions will return an error?  
```haskell
Prelude> 6 / 3
Prelude> 6 / length [1, 2, 3]
```  
The answer is the second, as the return type of `length` is _Int_, and does not implement the use of `/`.

4. How can we fix the broken code, using a different way of dividing?  
   Answer: `div 6 $ length [1, 2, 3]`

5. What is the type of the expression `2 + 3 == 5`?  
   Answer: type is `Bool` and the result is `True`.

6. What is the type and extected result value of the following:  
```haskell
Prelude> x = 5
Prelude> x + 3 == 5
```  
Answer: type is `Bool` and the result is `False`.

7. This one we will skip, but they are small and quite easy.

8. Write a function that tell you whether or not a given String (or list) is a palindrome. Use a predefined function called `reverse`, that reverses a String.
```haskell
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x
```

9. Write a function to return the absolute value of a number using if-then-else.
```haskell
myAbs :: Integer -> Integer
myAbs x = if x >= 0 then x else negate x
```

10. Fill in the definition of the following function, using `fst` and `snd`:
```haskell
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))
```

### Correcting syntax

1. Here we want a function that adds 1 to the lenght of a string argument and returns the result.
```haskell
x = (+)

F xs = w 'x' 1
  where w = length xs

-- FIXED

f xs = x w 1
  where w = length xs
```

2. This is supposed to be the identity function, `id`.
```haskell
\X = x

-- FIXED

id' = \x = x
```

3. When fixed this function will return 1 from the value (1, 2):
```haskell
F (a b) = A

-- FIXED:

f (a, b) = a
```

### Match the function names to their types

Gonna just write the (hopefully correct answers)

1. Which of the following types is the type of `show`?
```haskell
-- c)
Show a => a -> String
-- Answer is c!
```

2. Which of the folloing types is the type of `(==)`?
```haskell
-- b)
Eq a => a -> a -> Bool
-- Answer is b!
```

3. Which of the following types is the type of `fst`?
```haskell
-- a)
(a, b) -> a
-- Answer is a!
```

4. Which of the following types is the type of `(+)`?
```haskell
-- d)
(+) :: Num a => a -> a -> a
-- Answer is d!
```



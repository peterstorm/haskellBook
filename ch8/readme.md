# Chapter 8 - Recursion

## 8.1 Recursion

_Recursion_ is defining a function in terms of itself, via _self-referential_ expressions.
It means that the function will continue to call itself, until some condition is met to return a result.
It's a really important concept in Haskell, as is for example used to express _for loops_ from JavaScript for example.
It gives us means to not having to repeat ourselves, allowing the data we're processing to decide when we are done.  

In this chapter we will

* explore what recursion is and how recursive functions evaluate.
* go step-by-step through the process of writing recursive functions.
* have fun with _bottom_.

## 8.2 Factorial!

One of the classic introductory functions to recursion is factorial. In arithmetic you might have seen expressions like
_4!_. The _bang_ you're seeing next to the number 4 is the notation for the factorial function.

A silly way of implementing a very non interactive factorial function in Haskell is this:

```haskell
fourFactorial :: Integer
fourFactorial = 4 * 3 * 2 * 1
```

We want to make a factorial function we can pass an argument of what number to start with.

Our first try that we are going to demonstrate will be broken, to introduce the concept of a _base case_, kind of like
when you are pattern matching, you have to supply all the cases.

```haskell
brokenFact1 :: Integer -> Integer
brokenFact1 n = n brokenFact1 (n - 1)

-- lets apply 4 and see what happens:
brokenFact1 4 =
4 * (4 - 1)
* ((4 - 1) - 1)
* ((4 - 1) -1) -1)
-- ... this will never stop.
```

The way to stop a recursive function is to have a base case thta stops the self-application to further arguments. This is a
working implementation of factorial, using a base case.

```haskell
factorial :: Integer -> Integer
-- the base case
factorial 0 = 1
-- the recursive part
factorial n = n * factorial (n - 1)

functorial 4 =
4 * factorial (4 - 1)

-- evalute (-) applied to 4 and 1
4 * factorial 3

-- evaluate factorial applied to 3
-- expands to 3 * factorial (3 - 1), so we get
4 * 3 * factorial (3 - 1)

-- and so on
4 * 3 * factorial 2
4 * 3 * 2 * factorial (2 - 1)
4 * 3 * 2 * factorial 1
4 * 3 * 2 * 1 * factorial (1 - 1)
4 * 3 * 2 * 1 * factorial 0
4 * 3 * 2 * 1 * 1
24

-- because of the basecase, we end with a 1, because factorial 0 = 1

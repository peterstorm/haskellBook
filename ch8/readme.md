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

The way to stop a recursive function is to have a base case that stops the self-application to further arguments. This is a
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
```

### Another way to look at recursion

In the last chapter we looked at a higher-order function called composition. Function composition is when you take the output of one function and combine it with the input of another function. This is kind of the same thing recursion is doing, only the function taking the output, is the same function, and it is passed to itself again and again, until the base case
is reached.

We can write `(+1)` as a recursive function:

```haskell
incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times - 1) n)
```

Here `times` is a variable representing the number of times we want to increment the number `n`.
We can abstract out the recursion too, in an, atleast for me, kind of hard to wrap my head around way:

```haskell
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n - 1) f $ b

-- and we can use the new function inside incTimes' to specify how many times we want to (+1)
incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes times (+1) n
```

### Intermission: Exercise

Write out the evaluation of:

```haskell
applyTimes 5 (+1) 5
(+1) . applyTimes 4 (+1) $ 5
(+1) ((+1) . applyTimes 3 (+1) $ 5)
(+1) (+1) ((+1) . applyTimes 2 (+1) $ 5)
(+1) (+1) (+1) ((+1) . applyTimes 1 (+1) $ 5)
(+1) (+1) (+1) (+1) ((+1) . applyTimes 0 (+1) $ 5)
(+1) (+1) (+1) (+1) (+1) + 5
10
```

## 8.3 Bottom

⊥ or _bottom_ is a term used in Haskell to refer to computations that do not succesfully complete. The two main varieties of bottom are computations that failed with an error or those that failed to terminate. In logic ⊥ corresponds to false.
Let us examine a few ways by which we can have bottom in our programs:

```haskell
Prelude> let x = x in x
*** Exception: <<loop>>
```

Here, GHCi decided that the expression was never going to return and terminated the computation.
This an example of bottom because it was never going to return a result.  
Next let us define a function that will return an exception.

```haskell
f :: Bool -> Int
f True  = error "blah"
f False = 0

Prelude> f False
0
Prelude> f True
*** Exception: blah
```

We got an exception because we specified the program to return an error, but this too is an example of bottom.  
Another example of bottom is a partial function. If we rewrite the previous function:

```haskell
f :: Bool -> Int
f False = 0

Prelude> f False
0
Prelude> f True
*** Exception: 
      Non-exhaustive patterns in function f
```

We removed the explicit error, but we still get an exception, because the compiler knows that we did not write a
_total_ function, because we failed to define ways to handle all possible inputs. So how do we turn our function into
a total function. One way is with the use of the datatype `Maybe`.

```haskell
data Maybe a = Nothing | Just a
```

The `Maybe` datatype can take an argument. In the first case, `Nothing`, there is no argument. This is our way to say there is no result or data from the function without hitting bottom. The second case, `Just a`, takes an argument and allows us to return
the data we're wanting. `Maybe` makes all uses of nil/null values and most uses of bottom unnecessary.
Here's how we'd use it with our function:

```haskell
f :: Bool -> Maybe Int
f False = Just 0
f _     = Nothing
```

Note how both the type and both cases change, compared to our previous _partial_ implementation. We replaced the the error with Nothing but we also had to wrap our return value 0 in the `Just` constructor from `Maybe`. `Maybe` will return later.

## 8.4 Fibonacci numbers

Another classic demonstration of recursion in funcitonal programming, is a function that calculates the nth number in a
Fibonacci sequence. The Fibonacci sequence is a sequence of numbers in which each number is the sum of the previous two:
1, 1, 2, 3, 5, 8, 13, 21, 34... and so on. It's an indefinite computation that relies on two of it's own members, so perfect for recursion. We're going to walk through the steps of how we would write such a function, to get a better understanding of
the reasoning process.

1.
   Consider the types  
   The Fibonacci sequence only involves positive whole numbers, so the argument to the function is going to be the same.
   Our result will also be a positive whole number, so we would be looking at `Int` or `Integer`. We could use those
   concrete types or a typeclass for constrained polymorphism. Specifically we want a function that takes one integral argument
   and returns one integral result. So either:

   ```haskell
   fibonacci :: Integer -> Integer
   -- or
   fibonacci : Integral a => a -> a
   ```

2.
    Consider the base case  
    It can be difficult to determine the base case of your function, but it's worth thinking about. It is afterall a way
    to ensure that your function will terminate. Another thing is that it gives you a greater insight to understanding how
    the recursion in your function works. Fibonacci are positive numbers, so a reasonable base case is zero.
    The next number in the sequence is one, and zero plus one is one, so that is another reasonable base case.

    ```haskell
    fibonacci :: Integral a => a -> a
    fibonacci 0 = 0
    fibonacci 1 = 1
    ```

3.
    Consider the arguments  
    We want the argument to be the nth number in the Fibonacci sequence, such that if we pass it the number 10, we
    calculate the 10th number in the sequence. So we only need one parameter.
    This argument will also be used as an argument to the function within, because of the recursion, so to get the
    result of adding the two proceeding numbers, we also need to use `(x - 1) + (x - 2)`. The only reason I can see for this
    is because we can calculate pretty simple that it will work for the two base cases we have. I _cannot_ see what any other
    reason there is to come to the conclusion we need those two parentheses than that. The reasoning being, if we want to
    get the 6th Fibonacci number, we get it by adding the 5th and 4th number together, and that is like so:

    ```haskell
    fibonacci 6 = fibonacci (6 - 1) + fibonacci (6 - 2)
    -- which is
    fibonacci 6 = fibonacci 5 + fibonacci 4
    ```

    And that reasoning works all the way down to the base case, which is true, because:

    ```haskell
    fibonacci 2 = fibonacci (2 - 1) + fibonacci (2 - 2)
    -- which is
    fibonacci 2 = fibonacci 1 + fibonacci 0
    -- which in turn is
    fibonacci 2 = 1 + 0
    fibonacci 2 = 1
    -- because of our base cases.
    ```

    I stumpled upon it by luck, and only focusing on how the base case would solve the problem, which I find kind of
    annoying :D

## 8.5 Integral division from scratch

If we want to build a division function from scratch, we first need to consider the types we need. When we evaluate
10 / 5 to get the answer 2, 10 is the numerator, 5 the denominator, and 2 is the quotient. So we need atleast those
three types.  
So we could use the keyword `type` which declares a tyoe synonym or alias. We need them to just to Integer types, but
it is nice to keep them in their right place, so:

```haskell
type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

-- and then our function would have a type signature like so

dividedBy :: Numerator -> Denominator -> Quotient
dividedBy = undefined
```

The way you divide a number is basically to take the numerator, subtract the denominator and count how many times you can do
that, until your numerator is less than your denominator. We can write a recursive function that does that! We can also express if there is a remainder, by returning the result in a 2-tuple, like (quotient, remainder):

```haskell
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go num denom count
          | num < denom = (count, num)
          | otherwise   = go (num - denom) denom (count + 1)
```

We changed the type signature from earlier, to keep it more polymorphic, and to be able to return the tuple.
We also used a common Haskell idiom called a _go function_. This allows us to define a where-clause that can accept
more arguments than the top level function. Especially useful when we in this case, need to keep count of how many times
we have subtracted the denominator from our numerator! As it is a counter, it starts at 0, and is incremented in the
recursive function, everytime the _otherwise_ case is reached.

## 8.6 Chapter Exercises

### Review of types

1.
    What is the type of `[[True, False], [True, True], [False, False]]`?  
    d) `[[Bool]]`

2.
    Which of the following has the same type as `[[True, False], [True, True], [False, False]]`?  
    b) `[[3 == 3], [6 > 5], [3 < 4]]`

3.
    For the following function:  
    ```haskell
    func :: [a] -> [a] -> [a]
    func x y = x ++ y
    ```
    which of the following is true?  
    d) all of the above

4.
    For the `func` code above, which is a valid application of `func` to both of its arguments?  
    b) `func "Hello" "World"


### Recursion

1.
    Write out the steps for reducing `dividedBy 15 2` to its final answer, according to the Haskell code.
    ```haskell
    dividedBy 15 2 = (15 - 2) 2 (0 + 1)
                    = (13 - 2) 2 (1 + 1)
                    = (11 - 2) 2 (2 + 1)
                    = (9  - 2) 2 (3 + 1)
                    = (7  - 2) 2 (4 + 1)
                    = (5  - 2) 2 (5 + 1)
                    = (3 -  2) 2 (6 + 1)
                    = 1 < 2 = (7, 1)
    ```

2.
    Write a function that recursively suns all the numbers from 1 to n, n being the argument.
    The type should be `(Eq a, Num a) => a -> a`:
    ```haskell
    recSum :: (Eq a, Num a) => a -> a
    recSum 0 = 0
    recSum x = recSum (x - 1) + x
    ```

3.
    Write a function that multiplies to integral numbers using recursive summation. The type should be
    `Integral a => a -> a -> a`.
    ```haskell
    recMult :: Integral a => a -> a -> a
    recMult 0 _ = 0
    recMult _ 0 = 0
    recMult x y = go x y 1 0
      where go x y neg acc
              | x < 0     = if y < 0 then go (negate x) (negate y) 1 0 
                                     else go (negate x) y (-1) 0
              | x == 1    = if neg == (-1) then (y + acc) * (-1) 
                                           else (y + acc)
              | y == 1    = x + acc
              | otherwise = if neg == (-1) then go (x - 1) y (-1) (acc + y)
                                           else go (x - 1) y 1 (acc + y)
    ```

    This implementation is terrible, but as a first try I'm happy! Later I found out you could just have done this:
    ```haskell
    recMult'' :: Integral a => a -> a -> a
    recMult'' 0 _ = 0
    recMult'' x y
      | x < 0 = - recMult (-x) y
      | otherwise = y + recMult (x - 1) y
    ```

### Fixing dividedBy

Our `dividedBy` function wasn't quite ideal, as it was a partial function and did not return a result (bottom)
when given a denominator with 0 or less. We can fix that by building a datatype that handles the case of 0, and
change the function to work with negative values too.

```haskell
data DividedResult = Result Integer
                   | DividedByZero
                   deriving Show

fixedDividedBy :: Integral a => a -> a -> DividedResult
fixedDividedBy _ 0 = DividedByZero
fixedDividedBy nom denom = go num denom 1 0
  where go num denom neg acc
          | num < 0, denom < 0       = go (abs num) (abs denom) 1 0
          | num < 0                  = go (abs num) denom (-1) 0
          | denom < 0                = go num (abs denom) (-1) 0
          | num < denom, neg == (-1) = Result (-acc)
          | num < denom              = Result acc
          | otherwise                = go (num - denom) denom neg (acc + 1)
```

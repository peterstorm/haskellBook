# Chapter 2 - Hello Haskell
## 2.2 Interacting with Haskell code

_Prelude_ is a library of standard functions automatically loaded when opening
GHCi. To open GCHi in your Terminal (after installing _Stack_) type `stack ghci` at the
command line.

If something is _in base_ it means it is found in the Prelude.

### Working from source files

When working with Haskell code, normally a project will be comprised of _source files_, which are files denoted with the _.hs_ file extension.

[Writing a simple source file, looks like this](../ch2/test.hs)

To load up a source file in Prelude, you do the following:

```Haskell
-- load up the source file
Prelude> :load test.hs
-- run the "program"
Prelude> sayHello "Haskell"
-- output
Hello, Haskell!
Prelude>
```

## 2.3 Understanding expressions

Everything in Haskell is an expression. It can be values, combinations of values and/or functions applied to values.
They all evaluate to a result.

These are all expressions:

```Haskell
1
1 + 1
"Icarus"
```

First one is just a value that evaluates to itself. Second one evaluates to _2_ using the operator _+_ and the third one is a _string_ evaluating to itself.

## 2.4 Functions

Functions in Haskell are related to functions in mathematics. They map an input to an output.
A function is an expression that is applied to a result. Functions in Haskell take _one_ argument and returns _one_ result.
When passing multiple arguments to a function we are actually applying a series of nested functions, each to one argument.
This is called _currying_, more on that later.

Functions are how we factor out a pattern of what we would like to do multiple times, into reusable pieces, with varying
inputs, which in turn result in varying outputs.

For example, if we want a function that would multiply arguments by 3:

```Haskell
Prelude> triple x = x * 3
-- applying arguments
Prelude> triple 3
9
Prelude> triple 5
15
```

In Haskell you define a function in lowercase, and you apply arguments to a function using only a whitespace.

## 2.5 Exercises

Write a function that works for all the folllwing expressions:

```Haskell
3.14 * (5 * 5)
3.14 * (10 * 10)
3.14 * (2 * 2)
3.14 * (4 * 4)
```

The formula above looks like how you calculate the area of a circle.

```Haskell
cArea r = 3.14 * (r * r)
-- haskell has a value for _pi_ called _pi_, so we can rewrite it
cArea' r = pi * (r * r)
```

## 2.6 Infix operators

Functions in Haskell default to _prefix_ syntax, meaning the function being applied to an argument is at the beginning of the
expression. The _id_ function returns whatever it is given as an argument, and is prefix:

```Haskell
Prelude> id 1
1
Prelude> id "string"
"string"
```

Not all functions are prefix. Arithmetic operators like _+_ and _-_ are _infix_ and are _still_ functions. All operators
are function, but not all functions are operators.

```haskell
Prelude> 1 + 1
2
```

You can use functions that are usually _prefix_, with an _infix_ style, with a small change to the syntax:

```haskell
Prelude> 10 `div` 4
2
Prelude> div 10 4
2
```

`div` takes two arguments (currying) and as you can see above can be used infix style.

You can also use an _infix_ operator as a prefix operator by wrapping it in parentheses:

```haskell
Prelude> (+) 100 100
200
Prelude> (*) 3 3
9
```

### Associativity and precedence

Haskell is based on math, so the above is also true. We can ask GHCi for the `:info` about an operator or function. It tells
you all sorts of good information, and about associativity and precedence if applicable.

```haskell
:info (*)
infixl 7 *

:info (+) (-)
infixl 6 +
infixl 6 -
```

_*_ is left associative and has a higher precedence than _+_ and _-_ (higher number = higher precedence).
Left associativty means that an expression like `2 * 3 * 4` is evaluated from left to right, as in `(2 * 3) * 4`.

An example of right associativity is exponentiation:

```haskell
Prelude> :info (^)
infixr 8 ^
Prelude> 2 ^ 3 ^ 4
2417851639229258349412352
Prelude> 2 ^ (3 ^ 4)
2417851639229258349412352
(2 ^ 3) ^ 4
4096
```

## 2.7 Declaring values

When declaring values in a source file, the order of declaring the values does not matter, but doing it in the REPL does.

```haskell
Prelude> y = 10
Prelude> x = 10 * 5 + y
Prelude> myResult = x * 5
```

This makes sense, hopefully. In a source file like [learn.hs](../ch2/learn.hs) you can declare it all however you want, because
GHCi loads the entire file at once.

```
Then we have a whole section on style, which I will not go into - just remember WHITESPACE MATTERS!
```

```
And after the style guide, there is a whole section on modulus. Go read about it.
```

## 2.9 Parenthesization

The (_$_) operator is something that is frequently used. It's basically a way to stop having to write many, many
parentheses, first the definition:

`f $ a = f a`

If we `:info ($)` it, it returns `infixr 0 $`.

And as the example shows, you can use this operator to make sure everthing on the right of it (infixr) is evaluated first.

```haskell
Prelude> (2^) (2 + 2)
16
-- we can replace the parentheses
Prelude> (2^) $ 2 + 2
16
-- if we try without either the parens or $
Prelude> (2^) 2 + 2
6
```

You can also stack mulitple uses of _$_, like so:

```haskell
Prelude> (2^) $ (+2) $ 3 * 2
256
```

### Parenthesizing infix operators

As you've noticed in the previous section, we have been using operators like _^_ and _+_ in a weird way, namely something called
a _partially applied function_. If you use these operators as infix, like `(+)` you can partially apply it, by adding in
a number. So if you write `(+1)` the _+_ operator is partially applied with _1_ and will then add _1_ to whatever number you
pass it, because it's waiting for it's last argument. So:

```haskell
Prelude> (+1) 2
3
```

We also call it _sectioning_ when we pass around partially applied functions like `(+1)`. When it's a commutative function
like `(+)` it doesn't matter if we write it like `(+1)` or `(1+)` because the order of the arguments does not matter.
But if you use sectioning with a function that is not commutative, the order _does_ matter:

```haskell
Prelude> (1/) 2
0.5
Prelude> (/1) 2
2
```

Substraction is a special case, because if you write `(-2)` Haskell believes it to be an argument, or value. `(2-)` however
does work.

## 2.10 Let and where

There's a subtle difference between _let_ and _where_. _let_ introduces and _expression_ so it can be used wherever you can
have an expression, but _where_ is a _declaration_ and is bound to a surrounding syntactic context. Example:

```haskell
-- with where
module FunctionWithWhere where

printInc n = print plusTwo
 where plusTwo = n + 2
```

```haskell
-- with let
module FunctionWithLet where

printInc2 n = let plusTwo = n + 2
               in print plusTwo
```

### Exercises: A Head Code

1. `let x = 5 in x` evaluates to `5`.
2. `let x = 5 in x * x` evaluates to `25`.
3. `let x = 5; y = 6 in x * y` evaluates to `30`.
4. `let x = 3; y = 1000 in x + 3` evaluates to `6`.

Now rewrite som _let_ expressions to _where_ declarations, for example:

```haskell
-- in GHCi
let x = 5; y = 6 in x * y
-- could be rewritten as

-- put this in a file
multiply1 = x * y
  where x = 5
        y = 6
```

Rewrite with _where_ clauses:

```haskell
-- 1.
let x = 3; y = 1000 in x * 3 + y
-- becomes
example1 = x * 3 + y
 where x = 3
       y = 1000

-- 2.
let y = 10; x = 10 * 5 + y in x * 5
-- becomes
example2 = x * 5
 where x = 10 * 5 + y
       y = 10

-- 3.
let x = 7
    y = negate x
    z = y * 10
in z / x + y
-- becomes
example3 = z / x + y
 where x = 7
       y = negate x
       z = y * 10
```

## 2.11 Chapter Exercises

### Parenthesization

Attempt to reason the following expressions with parentheses:

```haskell
-- 1.
2 + 2 * 3 - 1
-- becomes
2 + (2 * 3) - 1

-- 2.
(^) 10 $ 1 + 1
-- becomes
(^) 10 (1 + 1)

-- 3. 
2 ^ 2 * 4 ^ 5 + 1
-- becomes
(2 ^ 2) * (4 ^ 5) + 1
```

### Equivalent Expressions

Attempt to reason which pairs of expressions will return the same result when evaluated:

```haskell
-- 1.
1 + 1
2
-- equivalent

-- 2.
10 ^ 2
10 + 9 * 10
-- equivalent

-- 3.
400 - 37
(-) 37 400
-- not equivalent

-- 4.
100 `div` 3
100 / 3
-- not equivalent

-- 5.
2 * 5 + 18
2 * (5 + 18)
-- not equivalent
```

### More fun with functions

Enter the follwoing expressions in the REPL:

```haskell
z = 7
y = z + 8
x = y ^ 2
waxOn = x * 5
-- waxOn = 1125
```

1. Now that you have a value called `waxOn` in the REPL, what will happen if you enter:
```haskell
Prelude> 10 + waxOn
1135
Prelude> (+10) waxOn
1135
Prelude> (-) 15 waxOn
-1110
Prelude> (-) waxOn 15
1110
```

2. Enter the `triple` function in the REPL, from earlier `triple x = x * 3`.

3. What do you think will happen if we enter `triple waxOn` in the REPL?
```haskell
Prelude> triple waxOn
3375
```

4. Rewrite `waxOn` as an expression with a _where_ clause in a source file.

5. To the same source file, add the `triple` function.

6. Now add a function called _waxOff_ in the file, `waxOff x = triple x`.

[Source file here.](../ch2/waxOn.hs)






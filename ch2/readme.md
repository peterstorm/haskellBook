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

## 2.7 Declaring valuesi

When declaring values in a source file, the order of declaring the values does not matter, but doing it in the REPL does.

```haskell
Prelude> y = 10
Prelude> x = 10 * 5 + y
Prelude> myResult = x * 5
```

This makes sense, hopefully. In a source file like [learn.hs](../ch2/learn.hs) you can declare it all however you want, because
GHCi loads the entire file at once.


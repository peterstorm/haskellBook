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


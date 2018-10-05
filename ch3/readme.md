# Chapther 3 - Strings
## 3.1 Printing strings

In this chapter we will:
* look at types to understand the datastructure called _String_.
* talk about the special syntax used for strings.
* print strings in the REPL environment.
* work with some standard functions that work with the datatype.

## 3.2 A first look at types

Types are a way of categorizing values. There are for example several types for numbers, whether they are integers, fractional, etc.
Theres the boolean type that specifically contains _True_ and _False_.
We will primarily look at _Char_ 'character' and _String_. Strings are lists of characters.
It's easy to see types in the REPL, enter the following:

```haskel
Prelude> :type 'a'
'a' :: Char
```

We encased the letter _a_ in single quotes as to tell the REPL that _a_ is not a variable, but a _character_.
Secondly the new syntax _::_ reads "has the type", so whenever you see double colon, you are looking at a _type signature_.

Now let's try and do the same wiht a _String_.

```haskell
Prelude> :type "Hello!"
"Hello!" :: [Char]
```

As we can see here, a _String_ is a list of _Chars_, that is what the brackets signify. _String_ is a _type alias_ or _type synonym_ for
a list of _Char_. A _type alias_ is what it sounds like, a name for a type, mostly for convenience,  that has a different type underneath.

## 3.3 Printing simple strings

In the REPL type:

```haskell
Prelude> print "Hello world!"
"hello world!"
```

`print` is a function we use to tell the REPL to print things to the screen, but it is not specific to strings, the following is however:

```haskell
Prelude> putStrLn "hello world!"
hello world!
```

`putStrLn` does not print it out to the screen with the quotes, we'll probably look at that later.
Next let's do this in a source file.

```haskell
-- print1.hs
module Print1 where

main :: IO ()
main = putStrLn "hello world!"
```

As you can see _main_ has the type _IO ()_. IO or I/O stands for input/output. In Haskell it is a special type called _IO_, used when the result of running a program involves
effects beyond evaluating a function or expression. Printing to the screen is an effect, so when printing the output of a module, it must be wrapped in this _IO_ type.

Now let's try another source file.

```haskell
-- print2.hs
module Print2 where

main :: IO ()
main = do
  putStrLn "Count to four for me:"
  putStr   "one, two"
  putStr   ", three, and"
  putStrLn " four!"
```

The output of main in this case will be:

```haskell
Prelude> main
Count to four for me:
one, twom three, and four!
Prelude>
```

**String concatenation**

To _concatenate_ something means to _link together_. Usually when we're concatenating in programming we are talking about linear sequences such as lists or strings of text.
For example `"Curry"` and `" Rocks"` becomes `"Curry Rocks"` if we concatenate them.

Now let's start a new file:

```haskell
-- print3.hs
module Print3 where

myGreeting :: String
myGreeting = "hello" ++ " world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO ()
main = do
  putStrLn myGreeting
  putStrLn secondGreeting
  where secondGreeting = 
          concat [hello, " ", world]
```

This little exercise demonstrates a few things:
1. We defined values at the top level of a module: (_myGreeting, hello, world_ and _main_). That is, they were declared at the top level so they are available throughout the module.
2. We specify explicit types for each top-level definition.
3. We concatenate strings with `(++)` and `concat`.

## 3.4 Top-level versus local definitions

Top-level definitions means that they are not nested within anything else, and they are in scope throughout the whole module.
A local definition is nested within some other expression and is not visible outside that expression.

Here's an example:

```haskell
-- topOrLocal.hs
module TopOrLocal where

topLevelFunction :: Integer -> Integer
topLevelFunction x = 
  x + woot + topLevelValue
  where woot :: Integer
        woot = 10

topLevelValue :: Integer
topLevelValue = 5
```

Here _topLevelValue_ and _topLevelFunction_ are both available to everything in the module, and could be imported to be used in a different module.
However `woot` is effectively invisible outsite of _topLevelFunction_.

**Exercises: Scope**

1. These lines of code are from a REPL session. Is _y_ in scope for _z_?
```haskell
Prelude> x = 5
Prelude> y = 7
Prelude> z = x * y
```
Answer: yes.

2. REPL session. Is _h_ in scope for _g_?
```haskell
Prelude> f = 3
Prelude> g = 6 * f + h
```

Answer: no.

3. This is from a source file. Is everthing we need to exceute _area_ in scope?
```haskell
area d = pi * (r * r)
r = d / 2
```

Answer: no.

4. This is from a source file. Is everything correct?
```haskell
area d = pi * (r * r)
  where r = d / 2
```

Answer: yes.

## 3.5 Types of concatenation functions

Lets look at _(++)_ and _concat_.

`++     has the type [a] -> [a] -> [a]`
and
`concat has the type [[a]] -> [a]`

So _concat_ takes a list of lists, which could be a list of strings or could literally be a list of lists, like:

```haskell
Prelude> concat [[1, 2], [3, 4, 5], [6, 7]]
[1, 2, 3, 4, 5, 6, 7]
Prelude> concat ["Iowa", "Melman", "Django"]
"IowaMelmanDjango"
```

The [_a_] in the type signature of _concat_ means that we have a list of a type _a_ we don't know yet, but it can be anything that can be in a list.
The signature also says that we cannot for example have a list of _String_ and `concat` it with a list of numbers, because the _a_ has to be the same.
Same thing applies with `(++)`.

## 3.6 Concatenation and scoping

Here we just have two examples, [one correct](../ch3/print3Flipped.hs) and [a wrong one](../ch3/print3Broken.hs) because of local definition. Fixed version is [here](../ch3/print3Fixed.hs).

## 3.7 More list functions

The `(:)` operator is called _cons_, and it builds lists.
```haskell
Prelude> 'c' : "hris"
"chris"
Prelude> 'P' : ""
"P"
```

`head` takes the first element of a list
```haskell
Prelude> head "Papuchon"
'P'
```

`tail` returns everything _but_ the head
```haskell
Prelude> tail "Papuchon"
"apuchon"
```

`take` takes a number of elements, starting from the left
```haskell
Prelude> take 1 "Papuchon"
"P"
Prelude> take 0 "Papuchon"
""
Prelude> take 6 "Papuchon"
"Papuch"
```

`drop` returns the remainder of the list after the specified number of elements has been dropped
```haskell
Prelude> drop 4 "Papuchon"
"chon"
Prelude> drop 9001 "Papuchon"
""
Prelude> drop 1 "Papuchon"
"apuchon"
```

The infix operator `(!!)`, returns the element that is in the specified position in the list.
Note that it uses _index zero_, so it starts from 0 and not `.
```haskell
Prelude> "Papuchon" !! 0
'P'
Prelude> "Papuchon" !! 4
'c'
```

All these functions are standard Prelude functions, but they are all considered _unsafe_. They are unsafe because they don't cover the case when they are given an empty list as an input.
Instead they throw an exception. More on this later.

## 3.8 Chapter Exercises

**Reading syntax**

Are the following lines of code correct? Check them in the REPL after your answer, and correct them if they are not correct.

1. `concat [[1, 2, 3], [4, 5, 6]]`  
   Correct.

2. `++ [1, 2, 3] [4, 5, 6]`  
   Not correct, fixed: `(++) [1, 2, 3] [4, 5, 6]`

3. `(++) "hello" " world"`  
   Correct.

4. `["hello" ++ " world"]`  
   Not correct. Fixed: `concat ["hello", " world"]`

5. `4 !! "hello"`  
   Not correct. Fixed: `"hello" !! 4`

6. `(!!) "hello" 4`  
   Correct.

7. `take "4 lovely"`  
   Wrong. Fixed: `take 4 "lovely"`  

8. `take 3 "awesome"`  
   Correct.

**Building functions**

Given the list manipulating functions mentioned, write functions that take the following input and return the expected outputs.

1. 
```
-- Given
"Curry is awesome"
-- Return
"Curry is awesome!"
-- Solution
"Curry is awesome" ++ "!"
```


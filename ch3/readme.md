#Chapther 3 - Strings
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

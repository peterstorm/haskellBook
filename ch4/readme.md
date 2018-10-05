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

**Exersices: Mood Swing**

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




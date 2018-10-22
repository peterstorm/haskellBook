# Chapter 9 - Lists

## 9.1 List

List can hold values, and enable to process the contents of the list in an easy way, and you can also generate
an infinite series of values if, usually created by a function, which allows them to act as a stream datatype.

In this chapter we will:
* explain list's datatyoe and how to pattern match on lists
* practice many standard library function for operating on lists
* learn about the underlying representation of lists
* see what that representation means for their evaluation
* and do a whole bunch of excersises!

## 9.2 The list datatype

The list datatype in Haskell is defined like this:

```haskell
data [] a = [] | a : [a]
```

`[]` is an empty list, and is a nullary data constructor, as it takes no arguments.
The second data constructor `(:)` has to arguments, and is an infix operator usually called _cons_, short for _construct_.  
The whole datatype is a sum type, because of the `|`, and the second data constructor within the data type is a _product_ type
because it takes two arguments. A sum type can be read as _or_ and a product type can be read as _and_.
The list datatype is a _sum_ type because it is either an _empty_ list or a list with something in it. It cannot be both.

## 9.3 Pattern matching on lists

We know we can pattern match on data constructors and the constructors for lists are no exceptions.  
Here we match on the first argument to _cons_, ignoring the rest of the list:

```haskell
Prelude> let myHead (x : _) = x
Prelude> :t myHead
myHead :: [t] -> t
Prelude> myHead [1, 2, 3]
1
```

We can do the opposite as well:

```haskell
Prelude> let myTail (_ : xs) = xs
Prelude> :t myTail
myTail :: [t] -> [t]
Prelude> myTail [1, 2, 3]
[2, 3]
```

We do need to be careful in with both functions though, if we pass an empty list as an argument, they will both fail.
We can fix it by putting in a base case in the case of `myTail` but the type signature of `myHead` makes the function not
typecheck, because `a` is not the same as an _empty list_.

```haskell
myHead :: [a] -> a
myHead []      = []
myHaed (x : _) = x
-- this wont work
```

### Using Maybe

A better way to handle the situation of something that might not be there, is to use the datatype called _Maybe_.
It's defined as `data Maybe a = Nothing | Just a`. Maybe is defined as returning `Nothing` if there is nothing to return and
`Just a` if there is. We can use it in both our functions.

```haskell
safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []       = Nothing
safeTail (x : []) = Nothing
safeTail (x : xs) = Just xs
```

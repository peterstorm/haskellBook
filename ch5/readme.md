# Chapter 5 - Types

## 5.1 Types

As we have seen, a datatype declaration defines a type constructor and data constructors.
Data constructors are the values of a particular type; they are also functions that let us create data, or values, of a particular type. although it will take some time before the full import of this becomes clear.
In Haskell, you cannot create untyped data, so except for a sprinkling of syntastic sugar for things like numbers or functions, everything originates in a data construtor from some definition of a type.

In this chapter we are going to look at:

* querying and reading type signatures
* see what _cyrrying_ is
* take a closer look at _polymorphism_
* type inference and how to declare types for our functions

## 5.2 What are types for?

_Types_ are kind of taken from type systems in logic and mathematics, and have been designed to impose constraints that
enforce correctness. A type system catches errors, so you for example can't add a number to a String and expect a valid result.
If a function expects to receive a Bool for example, and you give it a list, you will get a type error, thus saving you from errors for free.

## 5.3 How to read type signatures
### Understanding the function type

The arrow `(->)` is the type constructor for functions in Haskell. So it's a constructor type like `Bool` except the _type constructor_ takes arguments and has no _data constructors_.

```haskell
Prelude> :i (->)
data (->) a b
```

The next bit you should just read in the book.

### Typeclass-constrained type variables

If we look at the following:

```haskell
Prelude> :t (+)
(+) :: Num a => a -> a -> a
Prelude> :t (/)
(/) :: Fractional a => a -> a -> a
```

For `(+)` you can casually say that you need a numberic value (the constraint Num a), another numeric value and then it returns a numeric value, all of the same type, as if you give it one `a`, all the other `a`'s has to be the same.
For `(/)` the same applies, though here we are constrained by the fact that we need a value that implements the typeclass _Fractional_.

For a newbie right now, it's kind of hard to explain types, but the compiler will always give you the least specific type. And when you are constrained by a typeclass, it is because we don't yet know the concrete type of `a`.

_Num_ being the most generel typeclass, and then afterwards we could be more specific and call our a an _Int_ for example. Int will have an instance of the Num typeclass. A typeclass can also be seen as a class where that implements a number of standard functions. So Int, implementing Num, will have access to all the the functions of Num, like plus, minus, etc.

## 5.4 Currying

In other languages you can usally construct a function that takes multiple arguments. In Haskell, it might seem like you can too, but really there are syntactic conveniences that construct _curried_ functions. A curried function is a function that takes and argument, and with that argument returns another function that then takes another argument.
So when we look at a function like `f :: a -> a -> a` it might look like it takes two arguments, but really what happens is this
`f :: a -> (a -> a)`, because the `(->)` operator is right associative.
So the first _arrow_ takes an argument and then returns another function (the arrow inside the parentheses).

### Partial application

Partial application is related to currying and is when you apply a function to to less arguments than needed. So if a function takes two arguments, you can apply one argument to it, and save that application for later, and apply the last argument.
Example:
```haskell
addStuff :: Integer -> Integer -> Integer
addStuff a b = a + b

Prelude> addFive = addStuff 5
Prelude> :t addFive
addFive :: Num a => a -> a
Prelude> addFive 10
15
```

## Manual currying, currying and uncurrying existing functions, sectioning

I'll skip this, read it for yourselves.



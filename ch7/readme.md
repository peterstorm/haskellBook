# Chapter 7 - More functional patterns

## 7.1 Make it func-y

In this chapter we are going to look at and demonstrate that:

* Haskell functions are first-class entities that
* can be values in expressions, lists or tuples;
* can be passed as arguments to a function;
* can be returned from a function as a result;
* make use of syntactic patterns

## 7.2 Arguments and parameters

All Haskell values can be arguments to functions. A value like that is called a _first-class_ value.
Functions can be arguments to other functions in Haskell.

### Setting parameters

First we'll define a value with no parameters:

```haskell
myNum :: Integer
myNum = 1

myVal = myNum

-- if we quere the type of myVal

Prelude> :t myVal
myVal :: Integer
```

`myVal` has the same type as `myNum` because it is equal to it.
Now we introduce a parameter named f:

```haskell
myNum :: Integer
myNum = 1

myVal f = myNum

-- if we quere the type of myVal

Prelude> :t myVal
myVal :: t -> Integer
```

After we parameterized `myVal`, the type changes. The type `t` is polymorphic, because we don't do anything with it - it
could be anything! It inferred the maximally polymorphic type. If we do something with the `f` it will change the type:

```haskell
myNum :: Integer
myNum = 1

myVal f = f + myNum

-- if we quere the type of myVal

Prelude> :t myVal
myVal :: Integer -> Integer
```

Now it knows that `f` has to be of type _Integer_ because we add it to `myNum`.

If we add more parameters, to the original function without adding to `myNum`, the same thing happens as before:

```haskell
myNum :: Integer
myNum = 1

myVal f g = myNum

-- if we quere the type of myVal

Prelude> :t myVal
myVal :: t -> t1 -> Integer
```

Again, they infer the maximally polymorphic type, because we don't do anything with them.

### Binding varriables to values

Let's consider how the binding of variables works. Applying a function binds its parameters to values.
Type parameters become bound to a type, and a function variables are bound to a value.
The binding of values concerns not only the application of function arguments, but also things like `let` expressions and `where` clauses.

```haskell
addOne :: Integer -> Integer
addOne x = x + 1
```

When `addOne` is applied to a value we say that _x_ is now _bound_ to the value the function was applied to. Until a function's arguments have been applied, we cannot make use of the result of the function.

We can use `let` expressions to declare and bind variables as well:

```haskell
bindExp :: Integer -> String
bindExp x =
  let y = 5 in
  "the integer was: " ++ show x
  ++ " and y was: " ++ show y
```

In `show y` the `y` is only in scope _inside_ the `let` expression. This is _lexical scoping_ that means a variable declared in the innermost code, will be used, and will not be visible to any expression outside of `let` and `where` clauses.

## 7.3 Anonymous functions

An anonymous function is a function "_without a name"_. We usually use them, if we only need to use them once, and usually as a first-class function, as an argument to another function.

```haskell
triple :: Integer -> Integer
triple x = x * 3

-- can be written with anonymous syntax:

(\x -> x * 3) :: Integer -> Integer
```

We wrap it in parentheses to be able to apply arguments to the function, like `(\x -> x * 3) 5`.

### Exercises: Grab Bag

1. Which (two or more) of the following are equivalent?
   ```haskell
   -- a)
   mTh x y z = x * y * z
   -- b)
   mTh x y = \z -> x * y * z
   -- c)
   mTh x = \y -> z -> x * y * z
   -- d)
   mTh = \x -> \y -> \z -> x * y z
   ```
   They are all equivalent.

2. The type of the above function is `Num a => a -> a -> a -> a`
   Which is the type of `mTh 3`?

   The type is `Num a => a -> a -> a`

3. Rewrite the following:

   a.)

   ```haskell
   -- given
   addOneIfOdd n = case odd n of
    True -> f n
    False -> n
    where f n = n + 1

   -- into
   addOneIfOdd n = case off n of
    True -> f n
    False -> n
    where f = \n -> n + 1
   ```

   b.)

   ```haskell
   -- given
   addFive x y = (if x > y then y else x) + 5

   -- into
   addFive = \x -> \y -> (if x > y then y else x) + 5
   ```

   c.)

   ```haskell
   -- given
   mFlip f = \x -> \y -> f y x

   -- into
   mFlip f x y = f y x
   ```

## 7.4 Pattern matching

Pattern matching is _awesome_! 
Pattern matching matches against values or data constructors, _not_ types. Matching a pattern may fail, proceeding to the next available pattern to match or succeed. When a match succeeds, the variables exposed in the pattern are bound.
Pattern matching proceedes from let to right and from outside to inside.
We can pattern match on numbers, in the following example, if the _Integer_ argument to the function equals _2_, this will return `True`, otherwise `False`.

```haskell
isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False
```

Note the `_` after the match against the value `2`. This is a means of defining a universal pattern that never fails to match. If you give it _anything_ else than `2`, the function returns `False`. When you pattern match you should order your cases from most specific to least specific.






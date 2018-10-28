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

## 9.4 List's syntactic sugar

You can write a list in many ways, because of syntactic sugar.

```haskell
Prelude> [1, 2, 3, 4]
[1, 2, 3, 4]
Prelude> 1 : 2 : 3 : 4 : []
[1, 2, 3, 4]
Prelude> (1 : 2 : 3 : []) ++ (4 : [])
[1, 2, 3, 4]
```

When we talk about lists, we often talk about _cons cells_ and _spines_. Where cons cells are the data constructor that prepends more values recursively to "more lists".
The spine is the connective structure that holds the cons cells in place.o

## 9.5 Using ranges to construct lists

As seen above, there a several ways to construct lists, but we did leave out one of them which is with _ranges_. Here are some examples with range syntax.

```haskell
Prelude> [1..10]
[1,2,3,4,5,6,7,8,9,10]
Prelude> enumFromTo 1 10
[1,2,3,4,5,6,7,8,9,10]

Prelude> [1,2..10]
[1,2,3,4,5,6,7,8,9,10]
Prelude> enumFromThenTo 1 2 10
[1,2,3,4,5,6,7,8,9,10]

Prelude> [1,3..10]
[1,3,5,7,9]
Prelude> enumFromThenTo 1 3 10
[1,3,5,7,9]

Prelude> [2,4..10]
[2,4,6,8,10]
Prelude> enumFromThenTo 2 4 10
[2,4,6,8,10]

Prelude> ['t'..'z']
"tuvwxyz"
Prelude> enumFromTo 't' 'z'
"tuvwxyz"
```

The types of the functions underlying the range syntax are:

```haskell
enumFrom :: Enum a => a -> [a]

enumFromThen :: Enum a => a -> a -> [a]

enumFromTo :: Enum a => a -> a -> [a]

enumFromThenTo :: Enum a => a -> a -> a -> [a]
```

All of these require that the typed being "ranged" have an instance of the _Enum_ typeclass.

### Exercise: EnumFromTo

Some things you'll want to know about the _Enum_ typeclass:

```haskell
Prelude> :info Enum
class Enum a where
succ :: a -> a
pred :: a -> a
toEnum :: Int -> a
fromEnum :: a -> Int
enumFrom :: a -> [a]
enumFromThen :: a -> a -> [a]
enumFromTo :: a -> a -> [a]
enumFromThenTo :: a -> a -> a -> [a]
Prelude> succ 0
1
Prelude> succ 1
2
Prelude> succ 'a'
'b'
```

Write your own `enumFromTo` definitions for the types provided, but do not use range syntax to do so.

```haskell
eftBool :: Bool -> Bool -> [Bool]
eftBool x y
  | x > y     = []
  | x == y    = [x]
  | otherwise = [False, True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y = go x y []
  where go x y acc
          | x > y  = []
          | x == y =  reverse (x : acc)
          | otherwise = go (succ x) y (x : acc)

eftInt :: Int -> Int -> [Int]
eftInt x y = go x y []
  where go x y acc
          | x > y = []
          | x == y = reverse (x : acc)
          | otherwise = go (succ x) y (x : acc)

eftChar :: Char -> Char -> [Char]
eftChar x y = go x y []
  where go x y acc
          | x > y = []
          | x == y = reverse (x : acc)
          | otherwise = go (succ x) y (x : acc)
```

## 9.6 Extracting portions of lists

In this section we will look at some useful functions for extracting portions of a list, like:

```haskell
take :: Int -> [a] -> [a]
drop :: Int -> [a] -> [a]
splitAt :: Int -> [a] -> ([a], [a])
```

Here's how the take function works:

```haskell
Prelude> take 7 ['a'..'z']
"abcdefg"
Prelude> take 3 [1..10]
[1,2,3]
Prelude> take 3 []
[]
```

We can also use `take` with the range syntax from before. And remember we can create infinite lists in Haskell, and then with
`take` we just take a portion of that infinite list - lazyness! (I think)

```haskell
Prelude> take 10 (enumFrom 10)
[10,11,12,13,14,15,16,17,18,19]
```

The `drop` function is similar to `take`, though opposite, it drops the specified number of elements off the beginning of the
list.

```haskell
Prelude> drop 5 [1..10]
[6,7,8,9,10]
Prelude> drop 8 ['a'..'z']
"ijklmnopqrstuvwxyz"
Prelude> drop 4 []
[]
Prelude> drop 2 (enumFromTo 10 20)
[12,13,14,15,16,17,18,19,20]
```

The `splitAt` function cuts a list into two parts at the element specified by the `Int` and makes a tuple of two lists:

```haskell
Prelude> splitAt 5 [1..10]
([1,2,3,4,5],[6,7,8,9,10])
Prelude> splitAt 10 ['a'..'z']
("abcdefghij","klmnopqrstuvwxyz")
Prelude> splitAt 5 []
([],[])
Prelude> splitAt 3 (enumFromTo 5 15)
([5,6,7],[8,9,10,11,12,13,14,15])
```

The higher-order functions `takeWhile` and `dropWhile` are a bit different, as you can see from the type signatures:

```haskell
takeWhile :: (a -> Bool) -> [a] -> [a]
dropWhile :: (a -> Bool) -> [a] -> [a]
```

They take a function that returns a Bool, a _predicate_, and the functions will return elements of the list, while the
predicate holds true.

```haskell
Prelude> takeWhile (<3) [1..10]
[1,2]
Prelude> takeWhile (>6) $ enumFromTo 1 10
[]
-- returns an empty list, beause the predicate fails on the first element of the list
```

`dropWhile` drops the first part of the list while the predicate is met.

```haskell
Prelude> dropWhile (<3) [1..10]
[3,4,5,6,7,8,9,10]
Prelude> dropWhile (<8) (enumFromTo 5 15)
[8,9,10,11,12,13,14,15]
Prelude> dropWhile (>6) [1..10]
[1,2,3,4,5,6,7,8,9,10]
Prelude> dropWhile (=='a') "abracadabra"
"bracadabra"
```

### Exercises: Thy Fearful Symmetry

1.
    Using `takeWhile` and `dropWhile`, write a function that takes a string and returns a list of strings, using spaces to
    separate the elements of the string into words.
    ```haskell
    myWords :: String -> [String]
    myWords x = go x []
      where go x acc
              | x == ""   = reverse acc
              | otherwise = go (dropWhile (==' ') $ (dropWhile (/=' ') x)) ((takeWhile (/=' ') x) : acc)
    ```

2.
    Next, write a function that takes a string and returns a list of strings, using newline separators to break up the 
    string as in the following (your job is to fill in the undefined function):
    ```haskell
    module PoemLines where

    firstSen  = "Tyger Tyger, burning bright\n"
    seconSen  = "In the forests of the night\n"
    thirdSen  = "What immortal hand or eye\n"
    fourthSen = "Could frame thy fearful\
                \ symmetry?"

    sentenses :: String
    sentenses = firstSen ++ seconSen ++ thirdSen ++ fourthSen

    myLines :: String -> [String]
    myLines x = go x []
      where go x acc
              | x == "" = reverse acc
              | otherwise = go (dropWhile (=='\n') $ dropWhile (>'\n') x) (takeWhile (/='\n') x : acc)

    shouldEqual :: [String]
    shouldEqual =
      [ "Tyger Tyger, burning bright"
      , "In the forests of the night"
      , "What immortal hand or eye"
      , "Could frame thy fearful symmetry?"
      ]

    main :: IO ()
    main =
      print $
        "Are they equal? "
        ++ show (myLines sentenses == shouldEqual)
    ```

## 9.7 List comprehensions

List comprehensions are a way of generating a new list for a list or lists.
They come directly fomr the concept of set comprehensions in mathematics, including similar syntax. There's a _generator_, an
_output_ function and the _pipe_ that separates the input and the output function.

The following will square all the items in a list:

```haskell
[ x^2 | x <- [1..10]]

Prelude> [x^2 | x <- [1..10]]
[1,4,9,16,25,36,49,64,81,100]
```

### Adding predicates

You can add predicates to list comprehensions. Predicates have to evaluate to Bool. Only the list elements that meets the True
case will be passed to the output function.
Here we will only pass the even numbers to our output function:

```haskell
Prelude> [x^2 | x <- [1..10], rem x 2 == 0]
[4,16,36,64,100]
```

You can also write list comprehensions that have multiple list generators. One thing to note is that the right-most generator
will be exhausted first.

```haskell
Prelude> [ x^y | x <- [1..5], y <- [2,3]]
[1,1,4,8,9,27,16,64,25,125]
```

When we examine the output, we see that the first value of the list containing _x_, has been applied to the power of 2, then the
power of 3, then the next value of x is applied to the power of 2 and then 3, and so on.
We can, of course, also put a condition on that, if we for example only want to return all the values of the output
function are less than 200:

```haskell
Prelude> :{
Prelude| [x ^ y |
Prelude| x <- [1..10],
Prelude| y <- [2, 3],
Prelude| x ^ y < 200]
Prelude| :}
[1,1,4,8,9,27,16,64,25,125,36,49,64,81,100]
```

We can use multiple generators to turn two lit into a list of two-tuples, like so:

```haskell
Prelude> :{
Prelude| [(x, y) |
Prelude| x <- [1, 2, 3],
Prelude| y <- [6, 7]]
Prelude| :}
[(1,6),(1,7),(2,6),(2,7),(3,6),(3,7)]

Prelude> :{
Prelude| [(x, y) |
Prelude| x <- [1, 2, 3],
Prelude| y <- ['a', 'b']]
Prelude| :}
[(1,'a'),(1,'b'),(2,'a'),
(2,'b'),(3,'a'),(3,'b')]
```

You can also use list comprehensions in other list comprehensions:

```haskell
Prelude> let mySqr = [x^2 | x <- [1..10]]
Prelude> :{
Prelude| [(x, y) |
Prelude| x <- mySqr,
Prelude| y <- [1..3], x < 4]
Prelude| :}
[(1,1),(1,2),(1,3)]
```

### Exercises: Comprehend Thy Lists

Take a look at the following functions, figure out what you think their output lists will be and then run them in the REPL.

```haskell
[x | x <- mySqr, rem x 2 == 0]

[(x, y) | x <- mySqr,
          y <- mySqr,
          x < 50, y > 50]

take 5 [ (x, y) | x <- mySqr,
                  y <- mySqr,
                  x < 50, y > 50 ]
```

### List comprehension with Strings

It's worth remembering that strings are lists, so we can use list comprehension here as well.
We're going to introduce a standard function called `elem` that tells you weather an element is in a list or not.
It evaluates to a Bool value, so it's useful as a predicate in a list comprehension.

```haskell
Prelude> :t elem
elem :: Eq a => a -> [a] -> Bool
Prelude> elem 'a' "abracadabra"
True
Prelude> elem 'a' "Julie"
False
```

If we wanted to find all the capital letters of a string, we could do the following:

```haskell
Prelude> :{
Prelude| [x |
Prelude| x <- "Three Letter Acronym",
Prelude| elem x ['A'..'Z']]
Prelude| :}
"TLA"
```

We can turn that into a general function by making the list generator an argument to the function:

```haskell
Prelude> :{
Prelude| let acro xs =
Prelude| [x | x <- xs,
Prelude| elem x ['A'..'Z']]
Prelude| :}

Prelude> acro "Self Contained Underwater Breathing Apparatus"
"SCUBA"
Prelude> acro "National Aeronautics and Space Administration"
"NASA"
```

### Exercises: Square Cude

Given the following:

```haskell
Prelude> let mySqr = [x^2 | x <- [1..5]]
Prelude> let myCube = [y^3 | y <- [1..5]]
```

1.
    First write an expression that will make tuples of the outputs of the `mySqr` and `myCube`.
    ```haskell
    [(x, y) | x <- mySqr, y <- myCube]
    ```

2.
    Now alter the expression so that it only uses the x and y values that are less than 50.
    ```haskell
    [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

3.
    Now apply a function to the output of the previous list comprehension, that tells the number of tuples in the list.
    ```haskell
    length [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
    ```

## 9.8 Spines and nonstrict evaluation

As we have seen, lists are a recursive series of cons cells `a : [a]`, terminated bt the empty list.
We want a way to visualise the structure, to understand how lists gets processsed.
When we talk about data structures in Haskell, particulary lists, sequenses and trees, we talk about them having a _spine_.
The connective tissue that ties the collection of values together.

With lists, the spine is usually represented by the recursive cons operators, give the data: `[1, 2, 3]` we get a list
that looks like:

```
1 : 2 : 3 : []
or
1 : (2 : (3 : []))
 :
/ \
1  :
  / \
 2   :
    / \
   3  []
```

The problem with the `1 : (2 : (3 : []))` representation we used earlier is that it makes it seem like the value 1 exists 
“before” the cons (:) cell that contains it, but actually, the cons cells contain the values.
Because of this and the way nonstrict evaluation works, you can evaluate cons cells independently of what they contain. It is possible to evaluate only the spine of the list without evaluating individual values. It is also possible to evaluate only part of the spine of a list and not the rest of it.

Evaluation of the list in this representation proceeds down the spine. However, constructing the list (when that is necessary),
proceeds _up_ the spine. In the example above we start with an infix operator, evaluate the arguments 1 and a new cons cell,
and proceed downward to the 3 and the empty list. But when we need to build the list, and print it in the REPL, it proceeds
from the bottom, up the spine, putting the 3 in the empty list, then adding 2 to the front of the list and so on.

Because Haskell evaluation is non-strict, the list isn't constructed before we need it, before it is consumed. Nothing is, before it is needed.

The rest is more about the above concepts, and you should just read it.

## 9.9 Transforming lists of values

This is all about `map` and `fmap`, probably the the most iconic function of any functional language. It allows you to go into a _Functor_ (more on those later), and transform the values inside. One such functor is a list!

```haskell
Prelude> map (+1) [1, 2, 3, 4]
[2,3,4,5]
Prelude> map (1-) [1, 2, 3, 4]
[0,-1,-2,-3]
Prelude> fmap (+1) [1, 2, 3, 4]
[2,3,4,5]
Prelude> fmap (2*) [1, 2, 3, 4]
[2,4,6,8]
Prelude> fmap id [1, 2, 3]
[1,2,3]
Prelude> map id [1, 2, 3]
[1,2,3]
```

The types of `map` and `fmap` respectively are:

```haskell
map ::               (a -> b) -> [a] -> [b]
fmap :: Functor f => (a -> b) -> f a -> fb
```

Here's how `map` is defined in base:

```haskell
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs
```

Here we can see that we pattern match on the list argument to map. If we get an empty list, we do not care which function `f`,
we get, so we just throw it away with a `_` and return an empty list.
Now that we've handled that case we know that we atleast have a one element list, so we pattern match on the cons constructor
and apply f to the head, the `x`. Then we take the rest of the list and _map_ the function over it again - if we only have one
element in the list, the function returns the empty list, and then we have `f x : []`, and if not we have a recursive function.

The rest is again just more of how Haskell is lazy and just go read it.

## 9.10 Filtering lists of values



# Chapter 8 - Recursion

## 8.1 Recursion

_Recursion_ is defining a function in terms of itself, via _self-referential_ expressions.
It means that the function will continue to call itself, until some condition is met to return a result.
It's a really important concept in Haskell, as is for example used to express _for loops_ from JavaScript for example.
It gives us means to not having to repeat ourselves, allowing the data we're processing to decide when we are done.  

In this chapter we will

* explore what recursion is and how recursive functions evaluate.
* go step-by-step through the process of writing recursive functions.
* have fun with _bottom_.

## 8.2 Factorial!

One of the classic introductory functions to recursion is factorial. In arithmetic you might have seen expressions like
_4!_. The _bang_ you're seeing next to the number 4 is the notation for the factorial function.  

A silly

elm-random
==========

Library for pure pseudo-random number generation.

The Generator Library provides a strategy for generating pure pseudo-random
sequences specified by a seed.

To get started immediately, `import Generator` and `import Generator.Standard` 

# Generator.Standard API

Generator.Standard is an implemenation of Generator that uses
an implementation of the Portable Combined Generator of L'Ecuyer for 32-bit
computers.

[The Standard Generator](#the-standard-generator)

[Generating Functions](#generating-functions)

* [int32](#int32)
* [int32Range](#int32range)
* [float](#float)
* [floatRange](#floatrange)
* [listOf](#listof)

[Generators](#generators)

[Example Usage](#example-usage)

* [Generating Numbers](#generating-numbers)
* [Generating Custom Types](#generating-custom-types)

[Suggested Type Synonyms](#suggested-type-synonyms)

## The Standard Generator

The Standard.Generator module provides a generator that
is almost a direct translation from Haskell's
[System.Random](http://hackage.haskell.org/package/random-1.0.1.1/docs/System-Random.html)
module which is an implemenation of the Portable Combined Generator of
L'Ecuyer for 32-bit computers. It has a period of roughly 2.30584e18.


```haskell
generator : Int -> Generator Standard
```

Given a seed value, creates a standard Generator.
Using the same seed will yield repeatable results.

```
-- Create a random number generator. This must be threaded through the program.
-- This one is a "standard generator" (Generator.Standard) but other kinds of 
-- generators can be used.
gen : Generator Standard
gen = generator 42

-- Create two random integers named x and y.
(x, gen' ) = int gen
(y, gen'') = int gen'
```

The explicit use of a generator makes it possible to reproduce results
by using the same seed. In the example above the seed was 42, and every time
that code runs it will give the same values for `x` and `y`.


## Generating Functions

All of these functions take a generator as an argument and step it forward,
returning the new version in their result. You generally should use the updated
generator for subsequent computations.

#### int32
```haskell
int32 : Generator g -> (Int, Generator g)
```
Generate a 32-bit integer in range [minInt,maxInt] inclusive.

A conceivable use of it would be:

      int32 gen == (42, gen')


#### int32Range
```haskell
int32Range : (Int, Int) -> Generator g -> (Int, Generator g)
```
Generate an integer in a given range. For example, the expression
`int32Range (0,1) generator` will produce either a zero or a one. Note: the
randomness is only enough for values within the range [minInt32, maxInt32].
 Although this function will continue to produce values outside of the 
range [minInt32, maxInt32], sufficient randomness is not guaranteed.

#### minInt32
```haskell
minInt32 : Int
```
The maximum value for randomly generated for ints

#### maxInt32
```haskell
maxInt32 : Int
```
The minimum value for randomly generated for ints

#### float
```haskell
float : Generator g -> (Float, Generator g)
```

Generate a float between 0 and 1 inclusive.

A conceivable usage would be:

      float gen == (0.1, gen')

#### floatRange
```haskell
floatRange : (Float, Float) -> Generator g -> (Float, Generator g)
```
Generate a float in a given range.

A usage of it
would look something like this:

      floatRange (-10.0, 10.0) gen == (1.1618, gen') 

#### listOf
```haskell
listOf : (Generator g -> (a, Generator g)) -> Int -> 
         Generator g -> ([a], Generator g)
```
Generate a list of random values using a generator function.

```haskell
-- list of 10 floats in range (0,1):
 listOf float 10 gen

-- list of 42 integers in range [0,3]
listOf (int32Range (0,3)) 42 gen

-- create a ten by ten list of list of random integers
listOf (listOf int 10) 10 gen

-- If you make a function to create random strings,
-- you can even create lists of those!
string : Generator g -> (String, Generator g)
listOf string 5 gen
```

## Generators

You need a generator to actually create random values. Each generator has different
properties, so be sure to use one appropriate for your use case. If none fit your usage
you can define your own by creating somithing with the `Generator` type.

```haskell
type Generator state = {
  state : state,
  next  : state -> (Int, state),
  split : state -> (state, state),
  range : state -> (Int,Int)
}
```
 This provides a common strategy for generating random values
 so that you can create your
own generators. A generator must define the following fields:

 * The `state` field holds the current state of the generator.
 * The `range` operation yields the range of values returned by the generator.
 * The `next` operation returns an Int that is uniformly distributed in the
   range described by `range` (including both end points) and a new generator.
 * The `split` operation allows one to obtain two distinct random number
   generators. This is very useful in functional programs (For example, when
   passing a random number generator down to recursive calls), but very
   little work has been done on statistically robust implementations of split.

## Example Usage

#### Generating Numbers

```haskell
module Example where

import open Generator
-- Generator.Standard provides a concrete implementation of Generator that
-- has enough randomness for most purposes.
import open Generator.Standard

-- Generates a float between 0 and 1 using the standard generator seeded with 0.
-- Notice that the generating function returns a pair containing the generated
-- element along with the resulting nextGenerator. The nextGenerator can
-- be used to generate more elements in the sequence by suppling it
-- in place of (standard n).
generateFloat = 
  let (val, nextGenerator) = float (generator 0)
  in val

-- Generates a list of 10 ints ranging from 0 to 100 using the standard
-- generator seeded with 0.
-- Notice that listOf returns a pair containing the list of the
-- specified size along with the resulting nextGenerator. The 
-- nextGenerator could then be used to produce more elements in
-- the sequence at another time by supplying it in place of (standard n)
generateIntList = 
  let generatingFunction = int32Range (0, 100)
      (vals, nextGenerator) = listOf generatingFunction 10 (generator 0)
  in vals
     
-- Generates a list of 15 ints where the first 5 elements are in the range [0, 9]
-- the middle 5 elements are in the range [10, 19] and the last 5 elements
-- are in th range [20,29] using the standard generator seeded with 0.
-- Notice that the generation function listOf returns a pair containing
-- a list and the generator to produce the next part of the sequence. This
-- is then propogated to the next generator function.
generateMany =
  let initialGenerator = generator 0
      (first, generator') = listOf (int32Range (0, 9)) 5 initialGenerator
      (second, generator'') = listOf (int32Range (10, 19)) 5 generator'
      (third, _) = listOf (int32Range (20, 29)) 5 generator''
  in first ++ second ++ third
```

#### Generating Custom Types

```haskell
import open Generator
import open Generator.Standard

-- A basic data type
data Color = Red | Green | Blue

-- Generates random colors
color : Generator g -> (Color, Generator g)
color gen =
  let (i, gen') = int32Range (0, 2) gen
      cval = case i of
        0 -> Red
        1 -> Green
        2 -> Blue
  in (cval, gen')
     
-- Generates a list of 5 random colors     
generateColorList = listOf color 5 (generator 42)

```

#### Suggested Type Synonyms

As you get a better grasp of the module presented here, you may find it usefule
to use the following type synonyms.

```haskell
type StdGen = Generator Standard
type Random a = Generator StdGen -> (a, StdGen)
```

With the use of these, the type signatures become easier to write:

```haskell
int32 : Random Int
float : Random Int
listOf : Random a -> Int -> Random [a]
```

However, with these synonyms it is not immediately obvious what
arguments the functions expect nor is it obvious what they return. 
This will almost certainly be confusing to those unfamiliar
with this library and it is not recommended that they be used on exported
functions.

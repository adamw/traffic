module Random.Generator(Generator,
                 int32,
                 int32Range,
                 float,
                 floatRange,
                 listOf,
                 minInt32,
                 maxInt32) where

{-|
The Generator Library provides an interface for generating pure pseudo-random
sequences specified by a seed. This allows for repeatable sequences.

To get started immediately, check out the Generator.Standard module which is an 
implemenation of this interface that provides an implementation of the Portable 
Combined Generator of L'Ecuyer for 32-bit computers. This implementation 
provides enough randomness for most purposes.

```
-- Create a random number generator. This must be threaded through the program.
-- This one is a "standard generator" (Generator.Standard) but other kinds of 
-- generators can be used.
gen : Generator Standard
gen = standard 42

-- Create two random integers named x and y.
(x, gen' ) = int gen
(y, gen'') = int gen'
```

The explicit use of a generator makes it possible to reproduce results
by using the same seed. In the example above the seed was 42, and every time
that code runs it will give the same values for `x` and `y`.


# Generating Numbers

All of these functions take a generator as an argument and step it forward,
returning the new version in their result. You generally should use the updated
generator for subsequent computations.

@docs int32, float, int32Range, floatRange, listOf

# Generators

You need a generator to actually create random values. Each generator has different
properties, so be sure to use one appropriate for your use case. If none fit your usage
you can define your own by creating somithing with the `Generator` type.

@docs Generator

-}


{-| Generate a 32-bit integer in range [minInt32,maxInt32] inclusive. 

A conceivable use of it would be:

      int32 gen == (42, gen')
-}
int32 : Generator g -> (Int, Generator g)
int32 = int32Range (minInt32, maxInt32)

{-| Generate an integer in a given range. For example, the expression
`intRange (0,1) generator` will produce either a zero or a one. 

Note: the randomness is only enough for 32-bit values. Although this function 
will continue to produce values outside of the range [minInt32, maxInt32],
sufficient randomness is not guaranteed.
-}
int32Range : (Int, Int) -> Generator g -> (Int, Generator g)
int32Range (lo, hi) generator =
  if lo > hi
  then int32Range (hi, lo) generator
  else 
    let k = hi - lo + 1
        -- 2^31 - 87
        b = 2147483561
        n = iLogBase b k
        f n acc state =
            case n of
              0 -> (acc, state)
              _ -> let (x, state') = generator.next state
                   in  f (n - 1) (x + acc * b) state'
        (v, state') = f n 1 generator.state
    in  (lo + v `mod` k, { generator | state <- state' })

iLogBase : Int -> Int -> Int       
iLogBase b i =
    if i < b then 1 else 1 + iLogBase b (i `div` b)

{-| The maximum value for randomly generated for 32-bit ints -}
maxInt32 : Int
maxInt32 = 2147483647

{-| The minimum value for randomly generated for 32-bit ints -}
minInt32 : Int
minInt32 = -2147483648

{-| Generate a float between 0 and 1 inclusive. 

A conceivable usage would be:

      float gen == (0.1, gen')

-}
float : Generator g -> (Float, Generator g)
float = floatRange (0,1)

{-| Generate a float in a given range. A usage of it
would look something like this:

      floatRange (-10.0, 10.0) gen == (1.1618, gen') 
-}
floatRange : (Float, Float) -> Generator g -> (Float, Generator g)
floatRange (lo, hi) generator = 
    if lo > hi
    then floatRange (hi, lo) generator
    else 
      let (x, generator') = int32Range (minInt32, maxInt32) generator
          scaled = (lo+hi)/2 + ((hi-lo) / toFloat (maxInt32 - minInt32)) * toFloat x
      in (scaled, generator')

{-| Create a list of random values using a generator function.


      -- list of 10 floats in range (0,1):
      listOf float 10 gen

      -- list of 42 integers in range [0,3]
      listOf (intRange (0,3)) 42 gen

      -- create a ten by ten list of list of random integers
      listOf (listOf int 10) 10 gen

      -- If you make a function to create random strings,
      -- you can even create lists of those!
      string : Generator g -> (String, Generator g)
      listOf string 5 gen
-}
listOf : (Generator g -> (a, Generator g)) ->
         Int -> Generator g -> ([a], Generator g)
listOf = listOfHelp []

listOfHelp : [a] -> (Generator g -> (a, Generator g)) ->
             Int -> Generator g -> ([a], Generator g)
listOfHelp list generate n generator =
    if n < 1
    then (reverse list, generator)
    else
        let (value, generator') = generate generator
        in  listOfHelp (value :: list) generate (n-1) generator'
            
{-| Generator provides a common interface for number generators.
To create one, you must specify three components: next, split, range

 * The `state` field holds the current state of the generator.
 * The `next` operation returns an Int that is uniformly distributed in the
   range returned by genRange (including both end points), and a new generator.
 * The `split` operation allows one to obtain two distinct random number
   generators. This is very useful in functional programs (For example, when
   passing a random number generator down to recursive calls), but very
   little work has been done on statistically robust implementations of split.
 * The `range` operation yields the range of values returned by the generator.
-}
type Generator state = {
  state : state,
  next  : state -> (Int, state),
  split : state -> (state, state),
  range : state -> (Int,Int)
}
            
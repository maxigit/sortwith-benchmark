-- * Purpose
{-
Sorting a list using `on compare f` result in  f being called
each time a comparison is needed, ie O(n*log(n)) times
whereas it could be memoized and should be in theory
only be computed once for each value of the list.

We'll try here different alternatives and compare the performances.

= Problem
In order to have realistic benchmark results, we need to sort
a long list of thing.
Instead of entering manually a long list, we prefer to generate
a list of arbitary length and sort by a non-trivial but usefull function.

We use here a list of `Int`, and generate it's bit-reversal pertmutation.
Example 0 1 2 3 4 5 6 7 => 0 4 2 6 1 5 3 7

-} 
-- * Methods
-- We'll use `Criterion` to do the benchmark

-- import Criterion.Main

--  And of course we need the `on` function
import Data.Function(on)

--  and sort ....
import Data.List (sortBy, unfoldr)

import Data.Tuple (swap)
-- **  Bit-Reverse extraction
toBit :: Int -> [Int]
toBit n  = reverse $  unfoldr remainder n where
          remainder 0 = Nothing
          remainder x = Just . swap $ divMod x 2 

toBitR :: Int -> [Int]
toBitR n = reverse $ toBit n


-- ** Normal Methods
-- First, we need to verify how many times the function to compare on is called.

sort1 :: Int -> [Int]
sort1 n = sortBy (compare `on` toBitR) [0..(2^n)-1]

{-
== Precompute all values
== Use of Memoize library
-}

main:: IO ()
main = undefined

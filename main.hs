{-# LANGUAGE ScopedTypeVariables #-}
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

import Criterion.Main

--  And of course we need the `on` function
import Data.Function(on)

--  and sort ....
import Data.List (sortBy, unfoldr)
import GHC.Exts (sortWith)
import Control.Monad.ST
import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as V
import qualified Data.Vector.Algorithms.Intro as V

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

-- ** Precompute all values
sort2 ::  Int -> [Int]
sort2 n = sortOn toBitR [0..(2^n)-1]

sortOn :: Ord k => (a -> k) -> [a] -> [a]
sortOn f xs = map snd $ sortBy (compare `on` fst) [(f x, x) | x <- xs]

sort3 :: Int -> [Int]
sort3 n = sortWith toBitR [0..(2^n)-1]
-- ** Vector
sort4 :: Int -> [Int]
sort4 n = map snd  $ sortV (\x ->  (toBitR x, x)) [0..(2^n)-1]



sortV :: forall a b . Ord b => (a -> b) -> [a] -> [b]
sortV f xs = let
    v :: ST s (V.STVector s b) 
    v = do
        v <- V.new (length xs)
        zipWithM_ (\x i  ->
                    V.write v i (f x)
                  ) xs [0..]
        V.sort v
        return v
    in V.toList ( runST $ V.unsafeFreeze =<< v :: V.Vector b)
    
sort5 :: Int -> [Int]
sort5 n = sortV' (compare `on` toBitR)  [0..(2^n)-1]
sortV' :: forall a . (V.Comparison a) ->  [a] -> [a]
sortV' f xs = let
    v :: ST s (V.STVector s a) 
    v = do
        v <- V.new (length xs)
        zipWithM_ (\x i  ->
                    V.write v i x
                  ) xs [0..]
        V.sortBy f v
        return v
    in V.toList ( runST $ V.unsafeFreeze =<< v :: V.Vector a)

-- **  Use of Memoize library
--
-- ** Main

main:: IO ()
main = defaultMain
    [ bgroup ("sort 2^" ++ show n)  [ bench "normal" $ nf sort1 n
                     , bench "pre"    $ nf sort2 n
                     , bench "with"    $ nf sort3 n
                     , bench "vector-direct"    $ nf sort5 n
                     , bench "vector-pre"    $ nf sort4 n
                     ]
    | n <- [4, 8,12]]

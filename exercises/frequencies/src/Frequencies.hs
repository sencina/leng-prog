module Frequencies  (Frequency, frequencyMap, frequencies, Frequencies.insert, insertionSort) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Tuple(swap)
import Data.Map.Strict

type Frequency = (Int, Char)

frequencies::String -> [Frequency]
frequencies [] = []
frequencies string = insertionSort (Prelude.map swap (toList (frequencyMap string)))

frequencyMap::(Ord a) => [a] -> Map a Int
frequencyMap [] = Map.empty
frequencyMap (x:xs) = Map.insert x (counter + 1) (frequencyMap xs)
            where counter = Map.findWithDefault 0 x (frequencyMap xs)

insert::(Ord a) => a -> [a] -> [a]
insert a [] = [a]
insert a (x:xs)= if a <= x then a:x:xs else x: Frequencies.insert a xs

insertionSort:: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort [x] = [x]
insertionSort (x:xs) = Prelude.foldr (Frequencies.insert) [x] xs
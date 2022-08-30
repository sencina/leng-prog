module Frequencies  (Frequency, frequencyMap, frequencies, Frequencies.insert, insertionSort) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Tuple(swap)
import Data.Map.Strict

type Frequency = (Int, Char)

frequencies::String -> [Frequency]
frequencies = error "Implement it"

frequencyMap::(Ord a) => [a] -> Map a Int
frequencyMap list = frequencyMapAux list Map.empty

frequencyMapAux:: (Ord a) => [a] -> Map a Int -> Map a Int
frequencyMapAux [] map = map
frequencyMapAux (x:xs) map = frequencyMapAux xs (Data.Map.Strict.insertWith (+) x 1 map)

insert::(Ord a) => a -> [a] -> [a]
insert a [] = [a]
insert a (x:xs)= if a <= x then a:x:xs else x: Frequencies.insert a xs

insertionSort:: (Ord a) => [a] -> [a]
insertionSort (x:xs) = Frequencies.insert x (insertionSort xs)
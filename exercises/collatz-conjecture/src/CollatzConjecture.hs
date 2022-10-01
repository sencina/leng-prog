module CollatzConjecture (collatz) where

import qualified Data.Map as Map

collatz :: Integer -> Maybe Integer
collatz x = collatzAux x 0

collatzAux:: Integer -> Integer -> Maybe Integer
collatzAux 1 b = Just b
collatzAux 0 _ = Nothing
collatzAux a b = if (even a) then collatzAux c d else collatzAux e d
              where
                   c = div a 2
                   d = b+1
                   e = 3*a + 1

--Practica Parcial

code:: [(Char,Int)] -> String -> [Int]
code list s = map func s
  where func = getCode list

getCode:: [(Char, Int)] -> Char -> Int
getCode [] value = -1
getCode ((c,i):xs) value = if value == c then i else getCode xs value

encodeString:: [(Char, String)] -> String -> String
encodeString [] _ = []
encodeString _ [] = []
encodeString l toEncode = concat [code | chars <- toEncode, let code = Map.findWithDefault "" chars m]
    where m = insertIntoMap l

insertIntoMap:: [(Char, String)] -> Map.Map Char String
insertIntoMap [] = Map.empty
insertIntoMap l = Map.fromList l

bubbleSort :: Ord a => [a] -> [a]
bubbleSort = foldr swapTill []

swapTill:: Ord a => a -> [a] -> [a]
swapTill x [] = [x]
swapTill x (y:xs) = min x y : swapTill (max x y) xs

data Tree a = Leaf a | Node a (Tree a) (Tree a)

getMax:: Tree Int -> Int
getMax (Leaf a) = a
getMax (Node a l r) = max a (max (getMax l) (getMax r))

toBin:: Int -> [Int]
toBin 0 = [0]
toBin n | n `mod` 2 == 1 = toBin (n `div` 2) ++ [1]
        | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]

type Graph = [(Int,[Int])]

checkCycles:: Graph -> Bool
checkCycles g = checkCyclesAux g []

checkCyclesAux:: Graph -> [Int] -> Bool
checkCyclesAux [] _ = False
checkCyclesAux (node:nodes) [] = checkCyclesAux nodes [fst (node)]
checkCyclesAux (node:nodes) visited
    |  contains (snd node) visited = True
    |  otherwise = checkCyclesAux nodes visited

contains:: (Eq a) => [a] -> [a] -> Bool
contains _ [] = False
contains [] _ = False
contains (x:xs) (y:ys)
    | y == x = True
    | otherwise = (contains xs (y:ys)) || (contains (x:xs) ys)

quicksort:: Ord a => [a] -> [a]
quicksort [] = []
quicksort [a] = [a]
quicksort (x:xs) =  quicksort [y | y <- xs, y<x ] ++ [x] ++ quicksort [y | y <- xs, y>=x ]

mergeSort:: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort (x:xs) = merge (mergeSort firstHalf) (mergeSort secondHalf)
    where
        half = (length (x:xs) `div` 2)
        firstHalf = take half (x:xs)
        secondHalf = drop half (x:xs)

merge:: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y = x:(merge xs (y:ys))
    | otherwise = y:(merge (x:xs) ys)

selectionSort:: (Eq a, Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort (x:xs) = min:selectionSort (delete min (x:xs))
    where
        min = minimum (x:xs)
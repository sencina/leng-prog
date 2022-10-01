module Lists (member, union, intersection, difference,
              insert, insertionSort,
              binaryToDecimal, toDecimal, toDec, decimal,firsts,
              binaryAdd) where

import Data.Char(digitToInt)

member:: Int -> [Int] -> Bool
member _ []      = False
member e (x:xs)  = e == x || member e xs


union:: [Int] -> [Int] -> [Int]
union [] ys     = ys
union (x:xs) ys
  | member x ys = union xs ys
  | otherwise   = x : union xs ys

-- Remove Implementations, from, here on

intersection:: [Int] -> [Int] -> [Int]
intersection [] a = []
intersection (x:xs) ys = if member x ys then (x:intersection xs ys) else intersection xs ys

difference:: [Int] -> [Int] -> [Int]
difference [] a = []
difference (x:xs) ys = if member x ys then difference xs ys else (x: difference xs ys)

insert:: Int -> [Int] -> [Int]
insert e [] = [e]
insert e (x:xs) = if e<=x then (e: x: xs) else (x: insert e xs)

insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort [x] = [x]
insertionSort (x:xs) = insert x (insertionSort xs)

insertionSortFold :: [Int] -> [Int]
insertionSortFold [] = []
insertionSortFold [x] = [x]
insertionSortFold (x:xs) = foldr (insert) [x] xs

binaryToDecimal :: [Int] -> Int
binaryToDecimal xs = toDecimal 2 xs

toDecimal :: Int -> [Int] -> Int
toDecimal b [] = 0
toDecimal b (x:xs) = x * b^(length xs) + toDecimal b xs

toDec::Int -> String -> Int
toDec b x = toDecimal b (map digitToInt x)

-- Same as `toDec` But use a list comprehension

decimal::Int -> String -> Int
decimal  b x = toDecimal b [xInt | e <- x, let xInt = digitToInt e]

firsts::[a] -> [[a]]
firsts [] = []
firsts (x:xs) = [x] : map (x:) (firsts xs)

-- Given two String that represents numbers in binary implement the 'binaryAdd' function
-- DO NOT USE a predefined '+' operation

binaryAdd::String -> String -> String
binaryAdd a b = reverse (binaryAddAux (reverse a) (reverse b) '0')

bt:: Char -> Char -> Char-> (Char,Char)
bt a b carry
      | a == '0' && b == '0' && carry == '0' = ('0','0')
      | a == '0' && b == '0' && carry == '1' = ('1','0')
      | a == '0' && b == '1' && carry == '0' = ('1','0')
      | a == '0' && b == '1' && carry == '1' = ('0','1')
      | a == '1' && b == '0' && carry == '0' = ('1','0')
      | a == '1' && b == '0' && carry == '1' = ('0','1')
      | a == '1' && b == '1' && carry == '0' = ('0','1')
      | a == '1' && b == '1' && carry == '1' = ('1','1')

binaryAddAux:: String -> String -> Char -> String
binaryAddAux [] [] c = [c]
binaryAddAux xs [] c = binaryAddAux xs "0" c
binaryAddAux [] ys c = binaryAddAux "0" ys c
binaryAddAux (x:xs) (y:ys) c = result : binaryAddAux xs ys newCarry
          where (result, newCarry) = bt x y c




module Lists (member, union, intersection, difference,
              insert, insertionSort,
              binaryToDecimal, toDecimal, toDec, decimal,
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
insertionSort (x:xs) = insert $ insertionSort xs
    where insert [] = [x]
          insert (y:ys)
              | x < y = x : y : ys
              | otherwise = y : insert ys

binaryToDecimal :: [Int] -> Int
binaryToDecimal xs = toDecimal 2 xs

toDecimal :: Int -> [Int] -> Int
toDecimal b [] = 0
toDecimal b (x:xs) = x * b^(length xs) + binaryToDecimal xs
    
toDec::Int -> String -> Int
toDec = error "Implement it"


-- Same as `toDec` But use a list comprehension

decimal::Int -> String -> Int
decimal  = error "Implement it"

-- Given two String that represents numbers in binary implement the 'binaryAdd' function
-- DO NOT USE a predefined '+' operation

binaryAdd::String -> String -> String
binaryAdd  = error "Implement it"
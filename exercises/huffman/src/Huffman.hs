module Huffman  (huffmanTrie, encode, decode, Trie(..), Bit(..)) where


import Data.Map(Map)
import qualified Data.Map as Map
import Data.Tuple(swap)
import System.Environment

data Bit = F | T deriving (Eq, Show)
type Bits = [Bit]

data Trie a = Empty
            | Leaf a
            | Trie a :-: Trie a deriving (Eq, Show, Ord)

type Frequency = (Int, Trie Char)

huffmanTrie::String -> Trie Char
huffmanTrie [] = Empty
huffmanTrie s = snd $ head $ generateTries l
  where l = map convertToTrie (frequencies s)

generateTries :: [Frequency] -> [Frequency]
generateTries [] = []
generateTries [a] = [a]
generateTries (x:y:xs) = generateTries (insert (joinTries x y) xs)

joinTries:: Frequency -> Frequency -> (Int, Trie Char)
joinTries (i1, c1) (i2, c2) = ((i1+i2), (c1 :-: c2))

convertToTrie :: (Int, Char) -> (Int, Trie Char)
convertToTrie (i,c) = (i, Leaf c)

frequencies::String -> [(Int, Char)]
frequencies s = insertionSort(map swap (Map.toList (frequencyMap s)))

frequencyMap::(Ord a) => [a] -> Map a Int
frequencyMap [] = Map.empty
frequencyMap (x:xs) = Map.insert x (counter + 1) (frequencyMap xs)
            where counter = Map.findWithDefault 0 x (frequencyMap xs)

insert::(Ord a) => a -> [a] -> [a]
insert a [] = [a]
insert a (x:xs)
  |x < a = x: insert a xs
  |otherwise = a:x:xs

insertionSort:: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

encode :: String -> Trie Char -> Bits
encode [] _ = []
encode (x:xs) trie = concat (map func (x:xs))
    where trie = huffmanTrie (x:xs)
          func = findChar trie


findChar :: Trie Char -> Char -> Bits
findChar trie a = findCharAux a trie []

findCharAux :: Char -> Trie Char -> Bits -> Bits
findCharAux c (l :-: r) b = (findCharAux c l (b++[F]) ) ++ (findCharAux c r (b++[T]))
findCharAux c (Leaf a) b
    |c == a = b
    |otherwise = []

find::Bits -> Trie a -> a
find (x:xs) (Leaf a) = error "Not found"
find [] (Leaf a) = a
find (x:xs) (l :-: r)
    | x == F = find (xs) l
    | x == T = find (xs) r


decode::Bits -> Trie Char -> String
decode [] _ = []
decode b t= decodeAux b t t

decodeAux:: Bits -> Trie Char -> Trie Char -> String
decodeAux [] (Leaf a) _ = [a]
decodeAux (x:xs) (Leaf a) ogTrie = a:(decode (x:xs) ogTrie)
decodeAux (x:xs) (l :-: r) ogTrie
    | x == F = decodeAux (xs) l ogTrie
    | x == T = decodeAux (xs) r ogTrie


printIO::[a] -> IO()
printIO [] = return ()
printIO (x:xs) =

main:: IO()
main = do
  putStrLn "Enter a filename"
  fileName <- getLine

  if (length fileName == 0) then do
    putStrLn "Enter a filename"
    return ()
  else do
    file <- readFile (fileName)
    let fileLines = lines file
    let string = concat fileLines
    let charQuant = length string
    let encodedBits = simpleEncode string
    let compression = fractionalDivision (length encodedBits) (charQuant*7) * 100
    print compression
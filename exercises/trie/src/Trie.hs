module Trie  (Trie(..), left, right, find, decode, toList) where

import Bit
  
data Trie a  = Leaf a | (Trie a) :-: (Trie a) deriving (Eq,Show)

left::Trie a -> Trie a
left (Leaf _) = error "Is Leaf"
left (a:-:_) = a

right::Trie a -> Trie a
right (Leaf _) = error "Is Leaf"
right (_:-:a) = a

  
find::Bits -> Trie a -> a
find (x:xs) (Leaf a) = error "Not found"
find [] (Leaf a) = a
find (x:xs) (l :-: r)
    | x == F = find (xs) l
    | x == T = find (xs) r

decode::Bits -> Trie Char -> String
decode = error "Define it"

toList::Trie a -> [(a, Bits)]
toList = error "Define it"

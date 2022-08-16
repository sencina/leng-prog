module ListsPart2 (Bit(..), bitAt, charToBits, bits, queens) where

import Data.Char(ord)  
import Data.Bits(testBit)
  
data Bit = F | T  deriving (Eq, Show, Enum, Read)
type Bits = [Bit]

bitAt :: Int -> Char -> Bit
bitAt n c = if testBit (ord c) (8-n) then T else F 

charToBits :: Char -> Bits
charToBits char = [(bitAt x char) | x <- [0..7]]

bits::String -> Bits
bits xs = concat [charToBits x | x <- xs]

type Solution = [Int]

queens::Int -> [Solution]
queens = error "Implement It"
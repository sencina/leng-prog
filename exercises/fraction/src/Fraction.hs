module Fraction (add, sub, mul, divide, hcf) where

type Fraction = (Int, Int)

-- Implement the `add` Function

add :: Fraction -> Fraction -> Fraction
add f1 f2 = (fst(f1)*snd(f2) + fst(f2)*snd(f1),snd(f1)*snd(f2))

-- Implement the `sub` Function

sub :: Fraction -> Fraction -> Fraction
sub f1 f2 = add f1 f3
          where f3 = ((-1)*fst(f2),snd(f2))

-- Implement the `mul` Function

mul :: Fraction -> Fraction -> Fraction
mul f1 f2 = (fst(f1)*fst(f2),snd(f1)*snd(f2))

-- Implement the `divide` Function

divide :: Fraction -> Fraction -> Fraction
divide f1 f2 = ((fst(f1) * snd(f2)),(snd(f1)*fst(f2)))

-- Implement the highest common factor (hcf) (A.K.A greatest common divisor)

hcf :: Int -> Int -> Int
hcf f1 0 = f1
hcf f1 f2 = hcf f2 (rem f1 f2)
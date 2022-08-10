module CollatzConjecture (collatz) where

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

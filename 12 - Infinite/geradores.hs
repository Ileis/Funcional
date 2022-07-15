import Data.List
import Data.Maybe
-- GERADOR 01

-- Recursivo

gerador1 = gerador1' 0

gerador1' x | x == 0 = x : gerador1' (x + 1)
            | otherwise = x : (-x) : gerador1' (x + 1)

-- List Comprehension

gerador11 = foldr (\(a, b) acc -> a : b : acc) [] $ zip [-x | x <- [0..]] [y | y <- [1..]]

-- Unfoldr

gerador111 = unfoldr (\b -> if b <= 0 then Just (b, abs b + 1) else Just (b, negate b)) 0

-- Iterate

gerador1111 = iterate (\x -> if x <= 0 then negate x + 1 else negate x) 0

-- GERADOR 02

-- Recursivo

gerador2 = gerador2' 1

gerador2' x | x > 0 = x : gerador2' (negate (x + 1))
            | otherwise = x : gerador2' (abs x + 1)

-- List Comprehension

gerador22 = [if even x then negate x else x | x <- [1..]]

-- Unfoldr

gerador222 = unfoldr (\b -> if even b then Just (negate b, b + 1) else Just(b, b + 1)) 1

-- Iterate

gerador2222 = iterate (\x -> if x < 0 then negate x + 1 else negate (x + 1)) 1

-- GERADOR 03

-- Recursivo

gerador3 = gerador3' 1

gerador3' x = x : gerador3' (x*2)

-- List Comprehension

gerador33 = [2^x | x <- [0..]]

-- Unfold

gerador333 = unfoldr (\b -> Just(2^b, b + 1)) 0

-- Iterate

gerador3333 = iterate (* 2) 1

-- GERADOR 04

-- Recursivo

gerador4 0 = []
gerador4 x = x : gerador4 (x `div` 2)

-- List Comprehension

gerador44 x = takeWhile (> 0) [x `div` y | y <- gerador3]

-- Unfoldr

gerador444 x = unfoldr (\b -> if b >= 1 then Just(b, b `div` 2) else Nothing) x

-- Iterate

gerador4444 x = takeWhile (> 0) $ iterate (`div` 2) x
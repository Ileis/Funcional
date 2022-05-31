reverso [] = []
reverso (x:xs) = reverso xs ++ [x]

-- sem o operador ++

reverso1 [] = []
reverso1 (x:xs) = append' x (reverso1 xs)

append' y [] = [y]
append' y (x:xs) = x : append' y xs
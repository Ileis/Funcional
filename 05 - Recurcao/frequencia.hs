-- opcao 1

frequencia1 x xs = length (filter (==x) xs)

-- opcao 2

frequencia2 x xs = length [y | y <- xs, y == x]

-- opcao 3

frequencia3 x [] = 0
frequencia3 x (y:ys) | x == y = 1 + frequencia3 x ys
                     | otherwise = frequencia3 x ys

-- opcao 4

frequencia4 x xs = foldl (\acc y -> if y == x then acc + 1 else acc) 0 xs

-- frequencia4 1 [] == 0
-- frequencia4 4 [4] == 1
-- frequencia4 4 [5] == 0
-- frequencia4 4 [4,4] == 2
-- frequencia4 2 [4,4] == 0
-- frequencia4 5 [4,5,2,1,5,5,9] == 3
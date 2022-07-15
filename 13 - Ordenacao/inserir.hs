inserir x [] = [x]
inserir x [a]
    | x > a = [a, x]
    | otherwise = [x, a]
inserir x (a:b:xs)
    | x <= a = (x:a:b:xs)
    | x > a && x <= b = (a:x:b:xs)
    |otherwise = a : inserir x (b:xs)
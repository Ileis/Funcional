-- opcao 1
uniao [] ys = ys
uniao xs [] = xs
uniao xs (y:ys) | y `elem` xs = uniao xs ys
                | otherwise   = uniao (xs ++ [y]) ys

-- opcao 2

uniao' [] ys = ys
uniao' xs [] = xs
uniao' xs (y:ys) = uniao' (appendX' y xs) ys

appendX' n [] = [n]
appendX' n (x:xs) | n == x = x:xs
                  | otherwise = x : appendX' n xs
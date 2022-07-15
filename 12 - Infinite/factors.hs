ehPrimo x = ehPrimo' x [1..x] 0

listPrimos x = takeWhile (< x) $ filter ehPrimo [2..]

ehPrimo' _ [] qtdDiv | qtdDiv > 2 = False
                     | otherwise = True
ehPrimo' num (x:xs) qtdDiv | qtdDiv > 2 = False
                           | otherwise = if num `mod` x == 0 then ehPrimo' num xs (qtdDiv + 1) else ehPrimo' num xs qtdDiv

expoente x y = length $ tail $ takeWhile (\(c, d) -> d == 0) $ iterate (\(a, b) -> a `divMod` y) (x, 0)
divExp x y = last $ takeWhile (\(c, d) -> d == 0) $ iterate (\(a, b) -> a `divMod` y) (x, 0)

factors x = filter (\(a, b) -> b > 0) $ factors' x (listPrimos x)

factors' _ [] = []
factors' num (x:xs) | num > 1 = (x, expoente num x) : factors' (fst $ divExp num x) xs
                    | otherwise = []
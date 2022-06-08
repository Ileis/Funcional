--IN : Natural n e lista u
--OUT: Lista com os n menores elementos de u na ordem que aparecem em u
removeMaior :: (Ord a) => [a] -> [a]
removeMaior = removeMaior' True

removeMaior' :: (Ord a) => Bool -> [a] -> [a]
removeMaior' flag [] = []
removeMaior' flag all@(x:xs) | flag      = if x == maior then removeMaior' False xs else x : removeMaior' True xs
                             | otherwise = all
                             where maior = maximum all

menores y xs = if length xs > y then menores y (removeMaior xs) else xs
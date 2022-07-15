removerMaior :: (Ord a) => [a] -> [a]
removerMaior = removerMaior' True

removerMaior' :: (Ord a) => Bool -> [a] -> [a]
removerMaior' flag [] = []
removerMaior' flag all@(x:xs) | flag      = if x == maior then removerMaior' False xs else x : removerMaior' True xs
                             | otherwise = all
                             where maior = maximum all
--IN : Natural n e lista u
--OUT: Lista com os n menores elementos de u na ordem que aparecem em u
removeMaior :: (Ord a) => [a] -> [a]
removeMaior [] = []
removeMaior all@(x:xs) = if x == maior then xs else x : removeMaior xs
                       where maior = maximum all


menores y xs = if length xs > y then menores y (removeMaior xs) else xs
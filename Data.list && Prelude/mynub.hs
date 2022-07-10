mynub' xs = foldl (\acc x -> if elem x acc then acc else x : acc) [] xs
mynub xs = reverse $ mynub' xs
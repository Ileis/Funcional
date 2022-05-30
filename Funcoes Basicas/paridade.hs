paridade :: [Bool] -> Bool
paridade xs = odd (length [x | x <- xs, x])
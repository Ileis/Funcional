myConcatMap :: (a-> [b]) -> [a] -> [b]
myConcatMap f xs = foldl (++) [] $ map f xs
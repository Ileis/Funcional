myfilter :: (a -> Bool) -> [a] -> [a]
myfilter f [] = []
myfilter f xs = if f (head xs) then (head xs) : myfilter f (tail xs) else myfilter f (tail xs)
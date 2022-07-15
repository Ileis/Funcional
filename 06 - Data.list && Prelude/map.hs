
mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f x = f (head x) : mymap f (tail x)
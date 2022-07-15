selec _ [] = []
selec xs ys = (xs !! head ys) : selec xs (tail ys)
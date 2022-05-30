maior [] = error "lista vazia"
maior [x] = x
maior (x:xs) = max x (maior xs)
 
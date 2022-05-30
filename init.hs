primeiros [] = error "lista vazia"
primeiros [x] = []
primeiros (x:xs) = x : primeiros xs

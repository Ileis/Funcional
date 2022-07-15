text = ['0'..'9'] ++ ['A'..'Z']

base x y = reverse $ base' x y

base' 0 y = []
base' x y = text !! (x `mod` y) : base' (x `div` y) y

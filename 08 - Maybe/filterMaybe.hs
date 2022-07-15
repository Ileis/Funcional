import Data.Maybe

filterMaybe [] = []
filterMaybe (x:xs) = if isNothing x then filterMaybe xs else fromJust x : filterMaybe xs
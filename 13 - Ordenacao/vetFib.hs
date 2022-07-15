import Data.List
import Data.Maybe

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

vetFib x = take x $ unfoldr(\b -> Just(fibonacci b, b + 1)) 0
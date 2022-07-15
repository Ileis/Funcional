import Data.List
import Data.Maybe
import Data.Sequence (unfoldl)

separa' 0 = []
separa' x = x `mod` 10 : separa' (x `div` 10)

separa x = reverse $ separa' x

-- Unfoldr

separa2' x = unfoldr (\b -> if b > 0 then Just(b `mod` 10, b `div` 10) else Nothing) x
separa2 x = reverse $ separa2' x
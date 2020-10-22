import Data.Function

--primeListX = fix (filterXX [2..] . concat . (map (\x -> [x, x+x..])))

filterXX::[Int]->[Int]->[Int]
filterXX (x:xs) (y:ys) = if (x > y) then filterXX (x:xs) ys
                                    else if (x == y) then filterXX xs ys
                                                     else x:filterXX xs (y:ys)
filterXX xs _ = xs

primeListX = fix (map head . scanl minus [2..] . map (\x -> [x, x+x..]))

minus :: Ord a => [a] -> [a] -> [a]
minus = minusBy compare

minusBy :: (a -> b -> Ordering) -> [a] -> [b] -> [a]
minusBy cmp = loop
  where
     loop [] _ys = []
     loop xs [] = xs
     loop (x:xs) (y:ys)
       = case cmp x y of
          LT -> x : loop xs (y:ys)
          EQ ->     loop xs ys
          GT ->     loop (x:xs) ys
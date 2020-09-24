import Data.List

primeListX = eratosphen [2..]

eratosphen:: [Int] -> [Int]
eratosphen [] = []
eratosphen (x:xs) = x:eratosphen (filterXX xs [x,x+x..])

filterXX::[Int]->[Int]->[Int]
filterXX (x:xs) (y:ys) = if (x > y) then filterXX (x:xs) ys
                                    else if (x == y) then filterXX xs ys
                                                     else x:filterXX xs (y:ys)
filterXX xs _ = xs

--filterXXhelper::(Int->Int->Bool)->[Int]->Int->[Int]->[Int]
--filterXXhelper _ [] z ys = ys
--filterXXhelper func (x:xs) z ys = if not (func x z) then filterXXhelper func xs z (x:ys)
--                                                    else filterXXhelper func xs z ys

--isdivisibleby::Int->Int->Bool
--isdivisibleby 0 _ = False
--isdivisibleby 1 _ = False
--isdivisibleby _ 0 = False
--isdivisibleby _ 1 = False
--isdivisibleby x y = if (x `mod` y) == 0 then True
--                                        else False

foldlX::(a->a->a)->a->[a]->a
foldlX _ f [] = f
foldlX func f (x:xs) = foldlX func (func f x) xs

foldrX::(a->a->a)->a->[a]->a
foldrX _ f [] = f
foldrX func f (x:xs) = func x (foldrX func f xs)

unfoldPrimeX::Int->Maybe(Int,Int)
unfoldPrimeX 1 = Nothing 
unfoldPrimeX x = let z = unfoldPrimeXHelper x primeListX
                 in Just(z, x `div` z)

unfoldPrimeXHelper::Int->[Int]->Int
unfoldPrimeXHelper 1 _ = undefined
unfoldPrimeXHelper x (prime:primes) = if (x `mod` prime) == 0 then prime
                                                              else unfoldPrimeXHelper x primes

unfoldReverseBinaryX::Int->Maybe(Int, Int)
unfoldReverseBinaryX 0 = Nothing
unfoldReverseBinaryX n = Just(n `mod` 2, n `div` 2)
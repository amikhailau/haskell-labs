-- С синусом по Тейлору что-то не так, числа какие-то не те получаются
teilorsinhelper:: Int->Int->Float->Int->Float->Integer->Float->Float
teilorsinhelper 0 k x neg xpow kfact sum = sum
teilorsinhelper n 0 x neg xpow kfact sum = teilorsinhelper (n-1) 1 x (neg *(-1)) (xpow * x * x) 1 x
teilorsinhelper n k x neg xpow kfact sum = teilorsinhelper (n-1) (k+1) x (neg *(-1)) (xpow * x * x) (kfact * (toInteger (2 * k - 1)) * (toInteger (2 * k))) (sum + (((fromIntegral neg) * xpow) / (fromIntegral kfact)))

teilorsin:: Float->Int->Float
teilorsin x n = teilorsinhelper n 0 x 1 x 1 0

concatXX:: [[a]] -> [a]
concatXX [] = []
concatXX xss = concatXXHelper xss []

concatXXHelper:: [[a]]->[a]->[a]
concatXXHelper [] xs = xs
concatXXHelper (xs:xss) ys = concatXXHelper xss (concatX ys xs)

reverseX :: [a] -> [a]
reverseX [] = []
reverseX xs = reverseXHelper xs []

reverseXHelper :: [a] -> [a] -> [a]
reverseXHelper [] ys = ys
reverseXHelper (x:xs) ys = reverseXHelper xs (x:ys)

concatXHelper :: [a]->[a]->[a]
concatXHelper xs [] = xs
concatXHelper [] ys = ys
concatXHelper (x:xs) ys = concatXHelper xs (x:ys)

concatX :: [a]->[a]->[a]
concatX xs ys = concatXHelper (reverseX xs) ys

fmapX :: (a -> b) -> [a] -> [b]
fmapX func xs = fmapXHelper func xs []

fmapXHelper :: (a -> b) -> [a] -> [b] -> [b]
fmapXHelper _ [] ys = ys
fmapXHelper func (x:xs) ys = fmapXHelper func xs ((func x):ys)
module FPAufgaben5 where

-- :set +s

-- (<) :: Ord a => a -> a -> Bool

-- (Num a) => [a] -> [a]
-- (++[1..10])
-- (\list -> list ++ [1..10]) list

-- f :: Num a => a -> a
-- f x = (\x -> x + 1) x

-- f x y = (== 3) x y
-- Fehlerhaft weil:
-- (== 3) steht für (\x -> x == 3)
-- (\x -> x == 3) nimmt aber nur ein Argument x an.

-- g (x : xs) = (\x -> (++) [x] xs)
-- Es funktioniert vom Compiler her
-- Bespiel: g [1,2,3,4] 9 ergibt [9,2,3,4]
-- Die x als erstes Element in einer List gepattern matched
-- und das x in der Anonymen funktion sind nicht dieselben,
-- die gleiche Bennenung der variablen macht es sehr unleserlich.
-- Es gibt kein case für eine leere List [].

potenz :: Integer -> Integer -> Integer
potenz base 0 = 1
potenz base exp = base * potenz base (exp -1)

potenz_it :: Integer -> Integer -> Integer
potenz_it base 0 = 1
potenz_it base exp = potenz_it' base base exp
  where
    potenz_it' akk _ 1 = akk
    potenz_it' akk base exp = potenz_it' (base * akk) base (exp -1)

potenz_polymorph :: Integral a => a -> a -> a
potenz_polymorph base 0 = 1
potenz_polymorph base exp = base * potenz_polymorph base (exp -1)

quadrat :: Double -> Double
quadrat = (** 2)

quadrat2 :: Double -> Double
quadrat2 = (\base -> base ** 2)

dritte_Potenz :: Double -> Double
dritte_Potenz = (** 3)

dritte_Potenz2 :: Double -> Double
dritte_Potenz2 = (\base -> base ** 3)

-- :: RealFloat a => a -> a
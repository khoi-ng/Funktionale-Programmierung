module FPAufgaben6 where

import Data.Char

-- foldr (-) :: Num b => b -> [b] -> b
-- foldr (-)

-- (+2) :: Num a => a -> a

-- f :: Num a => [a] -> [a]
-- f [ ] = f [1]

-- f :: generic -> generic
-- f x = (\x -> x) x

-- (a -> b -> b) -> b -> t a -> b
-- (3 - ( 3 - (32 - 9)))
-- (3 - (3 - 32 + 9))
-- (3 - 3 + 32 - 9)
-- 23

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

foldrMap :: (t -> a) -> [t] -> [a]
foldrMap _ [] = []
foldrMap f xs = foldr (\el els -> f el : els) [] xs

take_while :: (a -> Bool) -> [a] -> [a]
take_while _ [] = []
take_while pred (x : xs) =
  if (pred x)
    then x : take_while pred xs
    else []

-- take_while_true :: (a -> Bool) -> [a] -> [a]
-- take_while_true _ [] = []
-- take_while_true pred (x : xs) =
--   if (pred x)
--     then x : take_while_true pred xs
--     else []

--take_while (\x -> x < 3) [1, 2, 1, 1, 4, 2, 5] zu [1, 2, 1, 1]

take_while' :: (a -> Bool) -> [a] -> [a]
take_while' pred xs =
  foldr
    ( \el els ->
        if pred el
          then el : els
          else []
    )
    []
    xs

-- removeSpace' :: [Char] -> [Char]
-- removeSpace' xs = map (\x -> if x /= ' ' then x else []) xs

removeSpace' :: [Char] -> [Char]
removeSpace' [] = []
removeSpace' (x : xs) =
  foldr
    ( \el els ->
        if el /= ' '
          then el : els
          else toUpper (head els) : tail els
    )
    []
    (toUpper x : xs)

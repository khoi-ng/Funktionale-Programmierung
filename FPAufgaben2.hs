module FPAufgaben2 where

mittelwert x y =
  fromIntegral (summe x y) / fromIntegral (anzahl x y)

summe x y
  | x >= y = y
  | otherwise = x + summe (x + 1) y

anzahl x y
  | x >= y = 1
  | otherwise = y + 1 - x

-- funktioniert für ganze Zahlen (negativ & positiv)
quersumme x =
  if x < 0
    then -1 * quersummeHelper (abs x)
    else quersummeHelper (abs x)

quersummeHelper x
  | x < 10 = x
  | otherwise = mod x 10 + quersummeHelper (div x 10)

bonbonBuyCount cent =
  bonbonBuyCountHelper cent 10 0

bonbonBuyCountHelper cent price count
  | cent < price =
    count
  | price == 100 =
    bonbonBuyCountHelper (cent - price) price (count + 1)
  | otherwise =
    bonbonBuyCountHelper (cent - price) (price + 10) (count + 1)
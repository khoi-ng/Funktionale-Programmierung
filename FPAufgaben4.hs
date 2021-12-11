module FPAufgaben4 where

fakultaet n = fakultaet' n 1
  where
    fakultaet' 0 akk = akk
    fakultaet' n akk = fakultaet' (n - 1) (n * akk)

fakultaet'' 0 akk = akk
fakultaet'' n akk =
  let akk' = n * akk
   in seq akk' (fakultaet'' (n - 1) akk')

fakultaet2 0 = 1
fakultaet2 n = n * fakultaet (n - 1)

f_rek :: Int -> Int
f_rek 0 = 0
f_rek n = n ^ 2 + f_rek (n - 1)

f_iter :: Int -> Int
f_iter n = f_akk' n 0
  where
    f_akk' 0 akk = akk
    f_akk' n akk = f_akk' (n - 1) (akk + n ^ 2)

fib_rek :: Int -> Int
fib_rek n
  | n <= 2 = 1
  | otherwise = fib_rek (n - 1) + fib_rek (n - 2)

-- 1 1 2 3 5 8 13
{- fib_iter :: Int -> Int
fib_iter n = fib_akk' n 0 1
  where
    fib_akk' 0 akk _ = akk
    fib_akk' n akk akk2 = fib_akk' (n - 1) akk2 (akk + akk2) -}

fib_iter :: Int -> Int
fib_iter n = fib_akk' n 0 1
  where
    fib_akk' n akk akk2
      | n <= 0 = akk
      | otherwise = fib_akk' (n - 1) akk2 (akk + akk2)

fib_akk2 :: Int -> Int
fib_akk2 n = fib_akk2' n 0 1
  where
    fib_akk2' 0 akk _ = akk
    fib_akk2' n akk akk2 =
      let akk' = akk + akk2
       in seq akk' (fib_akk2' (n -1) akk2 akk')

-- (<) :: Ord a => a -> a -> Bool

-- (++[1..10])

f :: Int -> Int
f x = (\x -> x + 1) x
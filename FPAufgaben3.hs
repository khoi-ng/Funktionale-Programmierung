module FPAufgaben3 where

listTuple :: [(Double, Char)]
listTuple = [(1.3, 'a')]

emptyList :: [a]
emptyList = []

unterListe :: Int -> Int -> [a] -> [a]
unterListe n m ls =
  take m (drop n ls)

fibListe :: Int -> [Int]
fibListe n
  | n < 0 = []
  | otherwise = fibListe (n - 1) ++ [fib n]

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib x = fib (x -1) + fib (x -2)

listeZuPaar :: [(String, Int)] -> ([String], [Int])
listeZuPaar [] = ([], [])
listeZuPaar (x : xs) =
  case x of
    (str, zahl) ->
      (str : fst (listeZuPaar xs), zahl : snd (listeZuPaar xs))

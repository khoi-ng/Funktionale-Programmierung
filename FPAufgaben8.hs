module FPAufgaben8 where

-- data Zahl = Eins | Zwei | Drei deriving (Eq)

-- plus :: Zahl -> Zahl -> Zahl
-- plus Eins Eins = Zwei
-- plus Zwei Eins = Drei
-- plus Eins Zwei = Drei
-- plus Zwei Zwei = Drei
-- plus _ _ = Drei

-- f :: [Zahl] -> Zahl -> Zahl
-- f xs y = if xs == [] then y else (plus x y)
--   where
--     x = head xs

-- 1. Fehler Pattern für die Funktion plus ist nicht vollständig
-- 2. Man versucht eine Liste von Zahl per (==) mit einer leeren Liste zu vergleichen, Zahl muss ableiten von Eq also "deriving (Eq)", muss bei der Definition von Zahl stehen.
-- 3. Fehler ist, dass versucht wird den Typen "Zahl" mit einer Variablen "x" per (:) zu einer List zu konkatinieren
-- 4. Fehler ist, dass die Funktion (+) nicht für Datatype Zahl definiert ist. Was wohl benutzt werden soll ist die plus Funktion.

data Tree v
  = EmptyTree
  | LeafTree v
  | BranchTree v (Tree v)

data BinaryTree v
  = Empty
  | Branch v (BinaryTree v) (BinaryTree v)
  deriving (Show)

test :: BinaryTree Int
test = Branch 5 (Branch 3 (Branch 2 Empty Empty) (Branch 4 Empty Empty)) (Branch 6 Empty Empty)

test2 :: BinaryTree Int
test2 = Empty

isInTree :: Eq v => v -> BinaryTree v -> Bool
isInTree value Empty = False
isInTree value (Branch val leftTree rightTree) =
  ( value == val
      || isInTree value leftTree
      || isInTree value rightTree
  )

treeToList :: BinaryTree v -> [v]
treeToList Empty = []
treeToList (Branch val leftTree rightTree) =
  (treeToList leftTree) ++ [val] ++ (treeToList rightTree)

test3 :: [Integer]
test3 = [2, 3, 4, 5, 6]

listToTree :: [v] -> BinaryTree v
listToTree [] = Empty
listToTree xs =
  Branch
    (xs !! half)
    (listToTree (take half xs))
    (listToTree (drop (half + 1) xs))
  where
    len = length xs
    half = len `div` 2
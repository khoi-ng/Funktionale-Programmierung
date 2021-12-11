module FPAufgaben9 where

data A = B | C deriving (Eq)

class K a where
  f :: a -> Bool
  f _ = False

instance K A where
  f x = x == C

aufruf :: Bool
aufruf = f B

-- 1. g ist nicht definiert in class K
-- 2. typ Signatur "f :: a -> Bool" ist in instance deklaration nicht erlaubt
-- 3. (==) Funktion ist für A nicht definiert, wir können deriving (Eq) auf data A anwenden

data WochenTag
  = Montag
  | Dienstag
  | Mittwoch
  | Donnerstag
  | Freitag
  | Samstag
  | Sonntag
  deriving (Show)

data BinaryTree v
  = Empty
  | Branch v (BinaryTree v) (BinaryTree v)

class ListenGenerator a where
  listeGenerieren :: a -> [String]

instance Show v => ListenGenerator (BinaryTree v) where
  listeGenerieren Empty = []
  listeGenerieren (Branch val leftTree rightTree) =
    (listeGenerieren leftTree) ++ [show val] ++ (listeGenerieren rightTree)

test :: BinaryTree Int
test = Branch 5 (Branch 3 (Branch 2 Empty Empty) (Branch 4 Empty Empty)) (Branch 6 Empty Empty)

test2 :: BinaryTree WochenTag
test2 = Branch Donnerstag (Branch Dienstag (Branch Montag Empty Empty) (Branch Mittwoch Empty Empty)) (Branch Freitag Empty Empty)
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Ch11 where

data Price = Price Integer deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)

data Manufacturer
  = Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline
  = PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle
  = Car Manufacturer Price
  | Plane Airline Size
  deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 1000)


isCar :: Vehicle -> Bool
isCar (Car _ _) = True 
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu


class TooMany a where 
  tooMany :: a -> Bool

instance TooMany Int where tooMany n = n > 42
newtype Cats = Cats Int deriving (Eq, Show, TooMany, Num)

instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

newtype Goats = Goats (Int, Int) deriving Show
instance TooMany Goats where tooMany (Goats (a, b)) = a + b > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany $ x + y   


data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill | Mac
  | Windows
  deriving (Eq, Show)
data ProgLang = Haskell | Agda | Idris| PureScript deriving (Eq, Show)
data Programmer = Programmer 
  { 
  os :: OperatingSystem, 
  lang :: ProgLang 
  } deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem] 
allOperatingSystems = [ GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill , Mac, Windows ]

allLanguages :: [ProgLang] 
allLanguages = [Haskell, Agda, Idris, PureScript]

programmers :: [Programmer]
programmers = [ Programmer { os = x, lang = y } | x <- allOperatingSystems, y <- allLanguages]

extractOss = map (\(Programmer { os = os, lang = lang }) -> os) programmers 


data BinaryTree a = 
    Leaf 
  | Node (BinaryTree a) a (BinaryTree a) 
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a 
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b <  a = Node (insert' b left ) a right 
  | b >  a = Node left a (insert' b right)

map' :: (a -> b) -> BinaryTree a -> BinaryTree b
map' _ Leaf = Leaf
map' f (Node left a right) = Node (map' f left) (f a) (map' f right)

t = insert' 3 (insert' 7 (insert' 5 Leaf))
test :: IO ()
test = 
  if map' (*2) t == Node (Node Leaf 6 Leaf) 10 (Node Leaf 14 Leaf) 
  then putStrLn "ok" 
  else error "nok"

inorder' :: BinaryTree a -> [a]
inorder' Leaf = []
inorder' (Node left a right) = (inorder' left) ++ [a] ++ (inorder' right)

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f b Leaf = b
foldTree f b (Node left a right) = foldTree f (foldTree f (f a b) left) right

testFoldTree = if foldTree (+) 0 t == 15 then print "ok" else print "nok"


upper :: Char -> Char
upper c = toEnum (fromEnum c - 32)

capitalizeWords :: String -> [(String, String)]
capitalizeWords = go . words 
  where
    go [] = []
    go (w@(c : cs) : ws) = (w, upper c : cs) : go ws


data Expr a = Lit a | Add (Expr a) (Expr a)

eval :: (Num a) => Expr a -> a 
eval (Lit a) = a
eval (Add a b) = eval a + eval b

printExpr :: (Show a) => Expr a -> String
printExpr (Lit n) = show n
printExpr (Add a b) = printExpr a ++ " + " ++ printExpr b

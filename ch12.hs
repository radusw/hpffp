module Ch12 where

notThe :: String -> Maybe String 
notThe word = case word == "the" of 
  True -> Nothing
  otherwise -> Just word

replaceThe :: String -> String
replaceThe text = foldl (++) "" (f maybes)
  where maybes = map notThe $ words text
        f [Just w]        = [w]
        f [Nothing]       = ["a"]
        f ((Just w) : ws) = (w ++ " ") : f ws
        f (Nothing : ws)  = "a " : f ws
        f _               = []


countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s =
  case words s of
    []  -> 0
    [w] -> 0
    ws  ->
      let
        classifiedWords = map classify ws
        count (IsThe, StartsWithVowel) acc = 1 + acc
        count _ acc                        = acc
      in
        foldr count 0 $ zip classifiedWords (tail classifiedWords)

data Classification = IsThe | StartsWithVowel | SomethingElse

classify :: String -> Classification
classify s
  | s == "the"        = IsThe
  | startsWithVowel s = StartsWithVowel
  | otherwise         = SomethingElse


startsWithVowel :: String -> Bool
startsWithVowel []     = False
startsWithVowel (c:cs) = isVowel c

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"


countVowels :: String -> Integer 
countVowels = go 0
  where go acc []     = acc
        go acc (c:cs) = go (acc + r) cs where r = if isVowel c then 1 else 0


data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat k
  | k < 0 = Nothing
  | otherwise = Just (go k)
  where
    go 0 = Zero
    go k = Succ (go $ k - 1)


myIterate :: (a -> a) -> a -> [a] 
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = 
  let
    go Nothing acc = acc
    go (Just (a, b')) acc = a : go (f b') acc
  in
    go (f b) []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\a -> Just (a, f a)) x


data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a =
  case f a of
    Nothing -> Leaf
    Just (l, m, r) -> Node (unfold f l) m (unfold f r)

treeBuild :: Integer -> BinaryTree Integer 
treeBuild n
  | n < 0 = Leaf
  | otherwise = unfold f 0 
  where f k = if (k == n) then Nothing else Just (k + 1, k, k + 1) 

module Ch9 where

import Data.Char

eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool False True  = [False, True]
eftBool True  False = []
eftBool True  True  = [True]

eftBool' :: Bool -> Bool -> [Bool]
eftBool' a b
  | a > b     = []
  | a == b    = [a]
  | otherwise = a : eftBool' (succ a) b


eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd a b
  | a > b     = []
  | a == b    = [a]
  | otherwise = a : eftOrd (succ a) b

eftInt :: Int -> Int -> [Int]
eftInt a b
  | a > b     = []
  | a == b    = [a]
  | otherwise = a : eftInt (succ a) b

eftChar :: Char -> Char -> [Char]
eftChar a b
  | a > b     = []
  | a == b    = [a]
  | otherwise = a : eftChar (succ a) b


mySplitter :: Char -> String -> [String]
mySplitter c s = case s of
  []    -> []
  (_:_) ->  let (h, t) = (takeWhile (/=c) s, dropWhile (/=c) s)
            in h : (mySplitter c (dropWhile (==c) t))


myFilter :: String -> [String]
myFilter = filter (\x -> x /= "the" && x /= "a" && x /= "and") . words


myZip :: [a] -> [b] -> [(a, b)]
myZip = myZipWith (,)


myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f (x:xs) (y:ys) = (f x y) : myZipWith f xs ys
myZipWith _ _ _           = []


capitalizeUntilIndex :: Int -> String -> String
capitalizeUntilIndex i s@(c:cs)
  | i > 0 = toUpper c : capitalizeUntilIndex (i - 1) cs
  | otherwise = s
capitalizeUntilIndex _ _ = []

capitalizeHead :: String -> Char
capitalizeHead = toUpper . head


caesar :: Int -> String -> String
caesar i s@(c:cs)
  | i /= 0 = (chr . (\x -> offset + mod (ord x + i - offset) alphabet)) c : caesar i cs
  | otherwise = s
  where
    offset = ord 'a'
    alphabet = ord 'z' - ord 'a' + 1
caesar _ _ = []

unCaesar :: Int -> String -> String
unCaesar i s = caesar (-i) s


myReverse :: (Foldable f) => f a -> [a]
myReverse = foldl (flip (:)) []

myFlatten :: [[a]] -> [a]
myFlatten = flatMap id

flatMap :: (a -> [b]) -> [a] -> [b]
flatMap f (a:as) = f a ++ flatMap f as
flatMap _ _ = []


myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy comp (a:as) =
  let loop (h:t) acc = loop t x where x = case comp h acc of
                                            GT -> h
                                            otherwise -> acc
      loop []    acc = acc
  in loop as a

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f as = myMaximumBy (flip f) as

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMaximumBy (flip compare)


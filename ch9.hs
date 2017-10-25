module Ch9 where

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
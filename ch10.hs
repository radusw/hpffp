module Ch10 where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

-- filterDbDate db = [t | (DbDate t) <- db]
filterDbDate :: [DatabaseItem] -> [UTCTime] 
filterDbDate items = foldl (\acc i -> acc ++ f i) [] items where
  f (DbDate time) = [time]
  f _ = []

-- filterDbNumber db = [n | (DbNumber n) <- db]
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = \items -> foldr (\i acc -> f i ++ acc) [] items where
  f item = case item of
    DbNumber time -> [time]
    otherwise -> []

-- mostRecent = maximum . filterDbDate
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent items = foldl (\acc i -> f acc i) (UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)) items where
  f acc (DbDate time) = case compare time acc of
    GT -> time
    otherwise -> acc
  f acc _ = acc 

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb items = let xs = filterDbNumber items
              in (fromIntegral . sum) xs / (fromIntegral . length) xs


-- http://chrisdone.com/posts/twitter-problem-loeb
water :: [Int] -> Int
water h =
  let w = zipWith min (scanl1 max h) (scanr1 max h)
      r = zipWith (-) w h
  in sum r


fibs = 1 : scanl (+) 1 fibs
fibs1 x = fibs !! (x - 1)
fibs2 n = take n fibs
fibs3 x = takeWhile (< x) fibs
nos = 1 : map (+1) nos
fact = scanl (*) 1 nos


stops  = "pbtdkg"
vowels = "aeiou"
words0 = concatMap (\x -> (concatMap (\y -> map (\z -> [x] ++ [y] ++ [z]) stops) vowels)) stops
words1 = [[x] ++ [y] ++ [z] | x <- stops, y <- vowels, z <- stops, x == 'p']

seekritFunc x = (/) (fromIntegral . sum $ map length (words x)) (fromIntegral . length . words $ x)


myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny = \f -> foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
myElem = \x -> myAny (== x)

myReverse :: (Foldable f) => f a -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap = \f -> foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter = \f -> foldr (\x acc -> if f x then x : acc else acc) []

squish :: [[a]] -> [a]
squish = foldr (++) []

-- flatMap / concatMap
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) [] 

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy comp lst@(a:as) = 
  let f = \x acc -> case comp x acc of
                      GT -> x
                      otherwise -> acc
  in foldr f (last lst) lst

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy comp lst@(a:as) = 
  let f = \acc x -> case comp acc x of
                      LT -> acc
                      otherwise -> x
  in foldr f (last lst) lst

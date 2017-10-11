{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List (intersperse)


naiveFact :: Integer -> Integer
naiveFact 0 = 1
naiveFact n = n * naiveFact (n - 1)

fact :: Integer -> Integer
fact n = 
  let loop !acc n
        | n <= 0    = acc
        | otherwise = loop (n * acc) (n - 1)
  in loop 1 n


applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n - 1) f $ b

f :: (Eq a, Num a) => a -> a -> a -> a -> a
f = \y -> \i -> \s -> \r -> (applyTimes y (\x -> (s + x) * r) i) 
f' y i s r = (f y i s r) - (f y i s 1)


fib :: Integer -> Integer
fib n = (fib' 0 1 n)
  where
  fib' a !b n
    | n <= 1    = b
    | otherwise = fib' b (a + b) (n - 1)



type Quotient = Integer
data DividedResult = Result Quotient | DividedByZero deriving Show
dividedBy :: Integral a => a -> a -> DividedResult
dividedBy num denom
  | denom == 0 = DividedByZero
  | otherwise = Result r
  where
  r = go num denom 0
  go n d count
    | abs n < abs d = count
    | d < 0         = go (-n) (-d) count 
    | n < 0         = go (n + d) d (count - 1)
    | otherwise     = go (n - d) d (count + 1)


summy :: (Eq a, Num a) => a -> a
summy n
  | n == 0 = 0
  | otherwise = n + summy (n - 1)

proddy :: (Integral a) => a -> a -> a
proddy a b
  | b == 0 = 0
  | b == 1 = a
  | b < 0  = if b == -1 then (-a) else (-a) + proddy (-a) ((-b) - 1)
  | b > 0  = a + proddy a (b - 1)


mc91 :: (Ord a, Num a) => a -> a
mc91 x
  | x > 100 = x - 10
  | otherwise = mc91 . mc91 $ x + 11


digitToWord :: Int -> String
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

digits :: Int -> [Int]
digits n = loop n []
  where
  loop n !acc
    | n == 0    = acc
    | otherwise = let (q, r) = n `divMod` 10
                  in loop q (r : acc)

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits


main :: IO ()
main = do 
  {-
  putStrLn "fib of ?"
  str <- getLine
  let n = read str :: Integer
  (putStrLn . show . fib) n
  -}
  --ghc ch8.hs && time ./ch8
  (putStrLn . show . fib) 1000000

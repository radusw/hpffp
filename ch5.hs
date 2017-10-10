module Ch5 where

import Data.List


f :: a -> a -> a -> a
f = undefined

x :: Char; x = undefined


wbReverse :: Foldable t => t a -> [a]
wbReverse = \xs -> foldl (flip(:)) [] xs

anyEven = any even [1,2..]

prefix xs ys = and (zipWith (==) xs ys)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

wbMin :: Ord c => [c] -> c
wbMin = \xs -> (head . sort) xs

myMin :: Ord c => [c] -> c
myMin = \xs -> head $ sort xs


i :: a -> a
i = id

c :: a -> b -> a
c = \a -> \b -> fst (a, b)
                               
c' :: a -> b -> b
c' = \a -> \b -> snd (a, b)
                                                                                            
r :: [a] -> [a]
r = tail

co :: (b -> c) -> (a -> b) -> (a -> c)
co = (.)

a :: (a -> c) -> a -> a
a = \g -> \a -> snd (g a, a)
                              
a' :: (a -> b) -> a -> b
a' = \g -> \a -> g a


p :: IO ()
p = do
  print $ 1 + 2
  putStrLn "10"
  print (negate (-1))
  print ((+) 0 blah)
    where blah = negate 1

fa :: Int -> String 
fa = undefined

ga :: String -> Char 
ga = undefined

ha :: Int -> Char
ha = \i -> (.) ga fa i -- ga $ fa i; (ga . fa) i

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge = \f -> \g -> \a -> fst $ g $ f a -- (fst . g . f) a 


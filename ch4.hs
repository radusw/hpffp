module Ch4 where

import Data.Maybe 

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood 
changeMood Blah = Woot
changeMood    _ = Blah

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer 
myAbs x = if x >= 0 then x else -x 


f :: (a, b) -> (c, d) -> ((b, d), (a, c)) 
f = \x -> \y -> ((snd x, snd y), (fst x, fst y))

g xs = w `x` offset
  where offset = 1
        x = (+)
        w = length xs

myId = \x -> x

h :: [a] -> Maybe a
h (x : xs) = Just x
h _ = Nothing

i = \(x:xs) -> x

j (a, b) = a

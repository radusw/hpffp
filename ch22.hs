{-# LANGUAGE InstanceSigs #-}

module Ch22 where

import Text.Show.Functions

newtype Reader r a = Reader { runReader :: r -> a } deriving (Show)

ask :: Reader a a 
ask = Reader id

myLiftA2 :: Applicative f => (a -> b -> c)-> f a -> f b -> f c 
myLiftA2 f a b = f <$> a <*> b

asks :: (r -> a) -> Reader r a 
asks f = Reader f
-- e.g. runReader (asks (+1)) 2

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ (f . ra)

instance Applicative (Reader r) where 
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r $ ra r

instance Monad (Reader r) where 
  return = pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb $ ra r) r


cartesian :: [a] -> [b] -> [(a,b)]
cartesian as bs = (,) <$> as <*> bs

bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

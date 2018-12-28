module Ch17 where

import Data.Monoid
import Control.Applicative

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where 
  fmap _ Nil          = Nil
  fmap f (Cons x xs)  = Cons (f x) (fmap f xs)

instance Applicative List where 
  pure a = Cons a Nil

  (<*>) Nil Nil               = Nil
  (<*>) _ Nil                 = Nil
  (<*>) Nil _                 = Nil
  (Cons f fs) <*> as          = (f <$> as) `append` (fs <*> as)

append :: List a -> List a -> List a 
append Nil         ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys


-- ch18
instance Monoid (List a) where
  mempty                 = Nil
  mappend Nil x          = x
  mappend x Nil          = x
  mappend (Cons x xs) ys = Cons x $ xs <> ys

instance Monad List where
  return            = pure
  Nil         >>= _ = Nil
  (Cons x xs) >>= f = f x <> (xs >>= f)


fold :: (a -> b -> b) -> b -> List a -> b 
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a 
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b 
flatMap f as = concat' $ f <$> as


take' :: Int -> List a -> List a 
take' _ Nil = Nil
take' n (Cons a as)
  | n <= 0 = Nil
  | n > 0  = Cons a $ take' (n-1) as


newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Functor ZipList' where 
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where 
  pure a = ZipList' $ pure a

  _                      <*> ZipList' Nil           = ZipList' Nil
  ZipList' Nil           <*> _                      = ZipList' Nil
  (ZipList' (Cons f fs)) <*> (ZipList' (Cons a as)) = ZipList' $ Cons (f a) t 
    where (ZipList' t) = (ZipList' fs) <*> (ZipList' as)


data Validation e a = Failure e | Success a deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success x) = Success (f x) 

instance Monoid e => Applicative (Validation e) where
  pure                            = Success

  (Failure e) <*> (Failure e') = Failure (e <> e')
  (Failure e) <*> _            = Failure e
  _           <*> (Failure e)  = Failure e
  (Success f) <*> (Success x)  = Success (f x)


data Person = Person String Int deriving (Eq, Show)

main :: IO ()
main =
  let partial = Success $ Person
      name = Success "Radu" -- Failure ["err 1"]
      age = Success 30 -- Failure ["err 2"]
  in putStrLn $ show $ (partial <*> name <*> age :: Validation [String] Person)


data Four a b c d = Four a b c d deriving (Eq, Show)
instance Functor (Four a b c) where
  fmap f (Four x w y z) = Four x w y (f z)
instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x                               = Four mempty mempty mempty x
  (Four x w y f) <*> (Four x' w' y' z) = Four (x <> x') (w <> w') (y <> y') (f z)

data Four' a b = Four' a a a b deriving (Eq, Show)
instance Functor (Four' a) where
  fmap f (Four' x w y z) = Four' x w y (f z)
instance Monoid a => Applicative (Four' a) where
  pure x                                 = Four' mempty mempty mempty x 
  (Four' x w y f) <*> (Four' x' w' y' z) = Four' (x <> x') (w <> w') (y <> y') (f z) 


stops, vowels :: String
stops = "pbtdkg" 
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos as bs cs = liftA3 (,,) as bs cs

module Ch20 where

import Data.Monoid

data Three a b c = Three a b c
instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

data Three' a b = Three' a b b
instance Foldable (Three' a) where
  foldMap f (Three' a b c) = (f b) <> (f c)

-- filterF even [1,2,3,4] :: Sum Int
filterF :: (
  Applicative f,
  Foldable t,
  Monoid (f a)
  ) => (a -> Bool) -> t a -> f a
filterF fn = foldMap (\x -> if fn x then pure x else mempty)
-- filterF fn t = foldr (\h acc -> if fn h then pure h <> acc else acc) mempty t

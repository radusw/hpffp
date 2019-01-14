{-# LANGUAGE UndecidableInstances #-}

module Ch21 where

import Control.Applicative
import Data.Monoid

newtype Identity a = Identity a deriving (Eq, Ord, Show)
instance Functor Identity where
  fmap f (Identity a) = Identity $ f a
instance Foldable Identity where
  foldMap f (Identity a) = f a
instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)
instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a
instance Foldable (Constant a) where
  foldMap _ _ = mempty
instance Traversable (Constant a) where
  traverse f (Constant a) = pure $ Constant a

data Optional a = Nada | Yep a
instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)
instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a
instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y)
instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs 

  foldr _ z Nil = z 
  foldr f z (Cons x xs) = f x (foldr f z xs) 

  foldl _ z Nil = z 
  foldl f z (Cons x xs) = foldl f (f z x) xs 
instance Traversable List where
--traverse _ Nil         = pure $ Nil
--traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs
  traverse f xs = foldl (\acc h -> Cons <$> f h <*> acc) (pure Nil) xs

listTest = traverse (\x -> if x == 3 then Nothing else Just (x*2)) $ Cons 1 (Cons 2 Nil)

data Three a b c = Three a b c deriving (Eq, Ord, Show)
instance (Functor (Three a b), Foldable (Three a b)) => Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

data Pair a b = Pair a b
instance Functor (Pair a) where 
  fmap f (Pair x y) = Pair x (f y)
instance Monoid a => Applicative (Pair a) where
  pure x = Pair mempty x 
  (Pair u f) <*> (Pair v x) = Pair (u `mappend` v) (f x)
instance Foldable (Pair a) where 
  foldMap f (Pair _ y) = f y 
  foldr f z (Pair _ y) = f y z
instance Traversable (Pair a) where 
  traverse f (Pair x y) = Pair x <$> f y

--Big
data Three' a b = Three' a b b deriving (Eq, Ord, Show)
instance (Functor (Three' a), Foldable (Three' a)) => Traversable (Three' a) where
  traverse f (Three' a b c) = Three' a <$> f b <*> f c

data S n a = S (n a) a deriving (Eq, Ord, Show)
instance (Functor (S n), Foldable (S n), Traversable n) => Traversable (S n) where
  traverse f (S x y) = S <$> traverse f x <*> f y

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Ord, Show)
instance Functor Tree where
  fmap _ Empty        = Empty
  fmap f (Leaf x)     = Leaf (f x)
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)
instance Foldable Tree where
  foldMap _ Empty        = mempty
  foldMap f (Leaf x)     = f x
  foldMap f (Node l x r) = (foldMap f l) <> (f x) <> (foldMap f r)
instance Traversable Tree where
  traverse _ Empty        = pure Empty
  traverse f (Leaf x)     = Leaf <$> f x
  traverse f (Node l x r) = liftA3 Node (traverse f l) (f x) (traverse f r)
--                          Node <$> traverse f l <*> f x <*> traverse f r

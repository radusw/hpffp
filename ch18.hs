module Ch18 where

import Control.Monad ((>=>) )

maybeExample = Just 1 >>= (\x -> (+x) <$> Just 2)


-- putStrLn "blah" *> putStrLn "another thing"
-- putStrLn "blah" >> putStrLn "another thing"


sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM -- Kleisli composition operator

askForAge :: IO Int
askForAge = getAge "Hello! How old are you? "


data Nope a = NopeDotJpg
instance Functor Nope where
  fmap _ _ = NopeDotJpg 
instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg 
instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg 

data PhhhbbtttEither b a = PLeft a | PRight b deriving (Eq, Show)
instance Functor (PhhhbbtttEither b) where
  fmap f (PLeft x)  = PLeft (f x)
  fmap f (PRight x) = PRight x
instance Applicative (PhhhbbtttEither b) where
  pure                      = PLeft
  (PRight f) <*> _          = PRight f
  _          <*> (PRight x) = PRight x
  (PLeft f)  <*> (PLeft x)  = PLeft (f x)
instance Monad (PhhhbbtttEither b) where
  return           = pure
  (PLeft x)  >>= f = f x
  (PRight x) >>= _ = PRight x


j :: Monad m => m (m a) -> m a -- join
j m = m >>= id 

l1 :: Monad m => (a -> b) -> m a -> m b -- liftM
l1 f m = m >>= (\x -> return $ f x)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c -- liftM2
l2 f m n = do
  a <- m
  b <- n
  return $ f a b

a :: Monad m => m a -> m (a -> b) -> m b -- flip ap
a m n = do
  a' <- m
  f  <- n
  return $ f a'

meh :: Monad m => [a] -> (a -> m b) -> m [b] -- traverse
meh [] _ = return []
meh (a:as) f = do 
  x <- f a 
  xs <- meh as f
  return $ x:xs

meh' :: Monad m => [a] -> (a -> m b) -> m [b] -- traverse
meh' xs f = foldr (\h acc -> f h >>= (\x -> acc >>= (\xs -> return $ x : xs))) (return []) xs

meh'' :: Monad m => [a] -> (a -> m b) -> m [b] -- traverse
meh'' xs f = sequence $ xs >>= (\x -> return $ f x)

flipType :: (Monad m) => [m a] -> m [a] -- sequence
flipType xs = meh xs id

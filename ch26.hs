{-# LANGUAGE InstanceSigs #-}

module Ch26 where

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad
import Data.Functor.Identity


newtype MyMaybeT m a = MyMaybeT { runMyMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MyMaybeT m) where
  fmap f (MyMaybeT ma) = MyMaybeT $ (fmap . fmap) f ma

{-
:t fmap (<*>) [Just 2]
fmap (<*>) [Just 2] :: Num (a -> b) => [Maybe a -> Maybe b]

(<*>) <$> [Just (*2)] <*> [Just 2]
[Just 4]
 
import Control.Monad.Trans.Maybe

MaybeT $ (<*>) <$> [Just (*2)] <*> [Just 2]
MaybeT [Just 4]
-}
instance (Applicative m) => Applicative (MyMaybeT m) where
  pure x = MyMaybeT (pure (pure x)) 
  (MyMaybeT fab) <*> (MyMaybeT mma) = MyMaybeT $ (<*>) <$> fab <*> mma

instance (Monad m) => Monad (MyMaybeT m) where
  return = pure

  (>>=) :: MyMaybeT m a -> (a -> MyMaybeT m b) -> MyMaybeT m b
  (MyMaybeT ma) >>= f = MyMaybeT $ do
      v <- ma
      case v of
        Nothing -> return Nothing
        Just y -> runMyMaybeT (f y)


newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

instance Applicative m => Applicative (EitherT e m) where
  pure a = EitherT $ (pure . pure) a
  (<*>) (EitherT f) (EitherT ema) = EitherT $ (<*>) <$> f <*> ema

instance Monad m => Monad (EitherT e m) where
  return = pure
  (>>=) (EitherT ema) f = EitherT $ ema >>= either (return . Left) (runEitherT . f)

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ swapEither <$> ema

swapEither :: Either e a -> Either a e
swapEither ea = case ea of
  Left e  -> Right e
  Right a -> Left a

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT amc bmc (EitherT amb) = amb >>= either amc bmc


newtype StateT s m a = StateT {
  runStateT :: s -> m (a, s)
}

instance Functor m => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ \s ->
    fmap (\(a, s') -> (f a, s')) $ sma s

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (<*>) (StateT smab) (StateT sma) = StateT $ \s -> do
    (ab, s') <- smab s
    fmap (\(a, s'') -> (ab a, s'')) $ sma s'

instance Monad m => Monad (StateT s m) where
  return = pure
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (>>=) (StateT sma) f = StateT $ \s -> do
    (a, s') <- sma s
    runStateT (f a) s'


{- 
type Parser a = String -> Maybe (a, String)
-}
type Parser = StateT String Maybe


rDec :: Num a => Reader a a 
rDec = reader $ flip (-) 1

rShow :: Show a => ReaderT a Identity String
rShow = reader show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> do
  putStrLn $ "Hi: " ++ show r
  return (r + 1)

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \r -> do
  putStrLn $ "Hi: " ++ show r
  return (show r, (r + 1))


isValid :: String -> Bool 
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- liftIO getLine 
  guard $ isValid v 
  return v

doExcite :: IO () 
doExcite = do
  putStrLn "say something excite!" 
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE" 
    Just e -> putStrLn ("Good, was very excite: " ++ e)

module Ch30 where

import Control.Exception 
import Control.Monad
import Control.Concurrent (forkIO, threadDelay)
import System.IO


data MyException = 
    MyException1 Int
  | MyException2 
  deriving (Eq, Show)

instance Exception MyException


toException' :: Exception e => IO (Either e a) -> IO (Either SomeException a)
toException' ioa = do
  result <- ioa
  case result of
    Left e -> return $ Left $ toException e
    Right ok -> return $ Right ok

catchArith' :: IO a -> IO (Either SomeException a) 
catchArith' = 
  let tryS :: IO a -> IO (Either ArithException a)
      tryS = try
  in toException' . tryS

catchAsync' :: IO a -> IO (Either SomeException a) 
catchAsync' = 
  let tryS :: IO a -> IO (Either AsyncException a)
      tryS = try
  in toException' . tryS

canICatch' :: Exception e => e -> IO (Either SomeException ())
canICatch' e = fmap (join . join) $ (catchAsync' . catchArith' . catchAsync') (throwIO e)


canICatchAll :: Exception e => e -> IO (Either SomeException ())
canICatchAll e = try $ throwIO e


catchArith :: ArithException -> Maybe SomeException
catchArith err = Just $ toException err

catchAsync :: AsyncException -> Maybe SomeException
catchAsync err = Just $ toException err

catchMy :: MyException -> Maybe SomeException
catchMy err = Just $ toException err

canICatch :: Exception e => e -> IO (Either SomeException ())
canICatch e = join <$> (tryJust catchMy $ tryJust catchArith $ throwIO e)

catchTest :: IO ()
catchTest = do
  canICatch DivideByZero >>= print
  canICatch (MyException1 1337) >>= print
  canICatch StackOverflow >>= print
  canICatch MyException2 >>= print


openAndWrite :: IO ()
openAndWrite = do
  print "0"
  h <- openFile "test.dat" WriteMode
  print "1"
  hPutStr h (replicate 10000000 '0' ++ "abc")
  print "2"
  hClose h
  print "done"

data PleaseDie =
  PleaseDie
  deriving Show

instance Exception PleaseDie

main :: IO ()
main = do
  threadId <- forkIO $ mask_ openAndWrite
  threadDelay 200
  throwTo threadId PleaseDie

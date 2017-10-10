module Ch7 where

data Employee = Coder | Manager | Veep | CEO deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO ()
employeeRank f e e' =
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> (flip reportBoss) e e'

-- compare alt
codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' = compare e e'


myAbs :: Integer -> Integer
myAbs x
  | x < 0 = (-x)
  | otherwise = x

avgGrade :: (Fractional a, Ord a) => a -> Char 
avgGrade x
  | y >= 0.9 = 'A' 
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | y < 0.59 = 'F'
  where y = x / 100


f x = take 10 . filter odd . enumFrom $ x
pf  = take 10 . filter odd . enumFrom


tensDigit :: Integral a => a -> a 
tensDigit x = d
  where
    (xLast, _) = x `divMod` 10
    (_, d) = xLast `divMod` 10

hundredsDigit :: Integral a => a -> a
hundredsDigit = tensDigit . (`div` 10)


foldBool :: a -> a -> Bool -> a
foldBool = error "Error: Need to implement foldBool!"

foldBool1 :: a -> a -> Bool -> a
foldBool1 x y cond =
  case cond of
    True -> x
    False -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y cond
  | cond = x
  | otherwise = y

foldBool3 :: a -> a -> Bool -> a
foldBool3 x y True = x
foldBool3 x y False = y


g :: (a -> b) -> (a, c) -> (b, c)
g = \f -> \t -> ((f . fst) t, snd t)


roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

main = do
  print (roundTrip 4 :: Float)
  print (id 4)


module Ch6 where

data Mood = Blah | Woot
instance Show Mood where
  show _ = "Blah"

data Mood2 = Blah2 deriving Show


class Numberish a where
  fromNumber :: Integer -> a 
  toNumber :: a -> Integer

newtype Age =
  Age Integer
  deriving (Eq, Show)
instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n

newtype Year =
  Year Integer deriving (Eq, Show)
instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n

sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed 
  where 
    integerOfA = toNumber a
    integerOfAPrime = toNumber a'
    summed = integerOfA + integerOfAPrime


data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun --deriving (Ord, Show)
instance Eq DayOfWeek where 
  (==) Mon Mon = True
  (==) Tue Tue = True
  Weds == Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False
instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _ = GT
  compare _ Fri = LT
  compare _ _ = EQ
-- day of week and numerical day of month
data Date =
  Date DayOfWeek Int
instance Eq Date where
  -- Date Thu 10 == Date Thu 10
  (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth') =
    weekday == weekday' && dayOfMonth == dayOfMonth'


data Identity a = Identity a
instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'


data TisAnInteger = TisAn Integer deriving Show
instance Eq TisAnInteger where
  (==) (TisAn a) (TisAn a') = a == a'

data TwoIntegers = Two Integer Integer deriving Show
instance Eq TwoIntegers where
  (==) (Two a b) (Two a' b') = a == a' && b == b'

data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt a') = a == a'
  (==) (TisAString a) (TisAString a') = a == a'
  (==) _ _ = False


data Pair a = Pair a a deriving Show
instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair a' b') = a == a' && b == b'

data Tuple a b = Tuple a b deriving Show
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

data Which a = ThisOne a | ThatOne a
instance (Eq a) => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) _ _ = False

data EitherOr a b = Hello a | Goodbye b deriving Show
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye a) (Goodbye a') = a == a'
  (==) _ _ = False


data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'


chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith = \f -> \a -> \b -> f b + fromInteger a 

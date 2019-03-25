module Ch28 where

{-
Difference List - http://h2.jaguarpaw.co.uk/posts/demystifying-dlist/ 
-}
newtype DList a = DL { unDL :: [a] -> [a] }

empty :: DList a
empty = DL id
{-# INLINE empty #-}

singleton :: a -> DList a
singleton = DL . (:)
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList = ($[]) . unDL
{-# INLINE toList #-}

infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)
{-# INLINE cons #-}

infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL (unDL xs . (x:))
{-# INLINE snoc #-}


{-
Queue in terms of list - From Okasaki's Purely Functional Data Structures
-}
data Queue a = Queue { 
  enqueue :: [a], 
  dequeue :: [a]
} deriving (Eq, Show)

-- adds an item
push :: a -> Queue a -> Queue a 
push a (Queue enq deq) = Queue (a:enq) deq

pop :: Queue a -> Maybe (a, Queue a) 
pop q = case (dequeue q, enqueue q) of
  (x:xs,    ys) -> Just (x, Queue ys xs)
  ([]  ,  y:[]) -> Just (y, Queue [] [])
  ([]  ,    []) -> Nothing
  ([]  ,    ys) -> Just (head a, Queue [] (tail a)) where a = reverse ys

{-
*Ch28> q0 = push 3 $ push 2 $ push 1 (Queue [] [])
*Ch28> q0
Queue {enqueue = [3,2,1], dequeue = []}
*Ch28> q1 = pop q
*Ch28> q1
Just (1,Queue {enqueue = [], dequeue = [2,3]})
*Ch28> q2 = q1 >>= (pop . snd)
*Ch28> q2
Just (2,Queue {enqueue = [], dequeue = [3]})
*Ch28> q3 = ((push 4) . snd) <$> q2
*Ch28> q3
Just (Queue {enqueue = [4], dequeue = [3]})
*Ch28> q4 = q3 >>= pop
*Ch28> q4
Just (3,Queue {enqueue = [4], dequeue = []})
*Ch28> q5 = q4 >>= (pop . snd)
*Ch28> q5
Just (4,Queue {enqueue = [], dequeue = []})
*Ch28> q6 = q5 >>= (pop . snd)
*Ch28> q6
Nothing
-}

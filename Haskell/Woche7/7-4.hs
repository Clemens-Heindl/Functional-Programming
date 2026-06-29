import Data.List
import Data.Maybe

data State s a = St (s -> (a, s))

type Table a = [a]
data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Eq, Show)

instance Functor (State s) where
  fmap f (St g) =
    St (\s ->
      let (a, s') = g s
      in (f a, s'))

instance Applicative (State s) where
  pure x = St (\s -> (x, s))

  (St sf) <*> (St sa) =
    St (\s ->
      let (f, s1) = sf s
          (a, s2) = sa s1
      in (f a, s2))

instance Monad (State s) where
  return x = St (\s -> (x, s))
  (St m) >>= f = St (\s -> let (x, s1) = m s
                               St g = f x
                           in g s1)

monRenTree :: Eq a => Tree a -> State (Table a) (Tree Integer)

monRenTree (Leaf x) =
  St $ \tab ->
    case elemIndex x tab of
      Just i  -> (Leaf (fromIntegral (i + 1)), tab)
      Nothing -> (Leaf (fromIntegral (length tab + 1)), tab ++ [x])

monRenTree (Node x l r) =
  St $ \tab ->
    let (ix, tab1) =
          case elemIndex x tab of
            Just i  -> (i + 1, tab)
            Nothing -> (length tab + 1, tab ++ [x])

        St fl = monRenTree l
        St fr = monRenTree r

        (l', tab2) = fl tab1
        (r', tab3) = fr tab2
    in (Node (fromIntegral ix) l' r', tab3)
    
mRT :: (Eq a, Show a) => Tree a -> Tree Integer
mRT = extract . monRenTree

extract :: (Eq a, Show a) => (State (Table s) a) -> a
extract (St f) = fst (f [])
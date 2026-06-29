import Data.List
data TripleTree a = Leaf a | Node a (TripleTree a) (TripleTree a) (TripleTree a) deriving (Eq, Show)

numberTreeMF :: (Eq a, Show a) => TripleTree a -> TripleTree Int
numberTreeMF t =  fst (renameTreeMF t 1)

renameTreeMF :: Eq a => TripleTree a -> Int -> (TripleTree Int, Int)
renameTreeMF (Leaf x) next = ((Leaf next), next+1)
renameTreeMF (Node root l m r) n =
  let
      (l', n1) = renameTreeMF l n
      (m', n2) = renameTreeMF m n1
      (r', n3) = renameTreeMF r n2
  in (Node n3 l' m' r', (n3+1))
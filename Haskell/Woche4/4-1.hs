data DOrd = Infix | Prefix | Postfix | GspInfix | GspPrefix | GspPostfix deriving Eq
data Tree = Nil | Node Int Tree Tree

normalise :: DOrd -> DOrd
normalise GspInfix = Infix
normalise GspPrefix = Prefix
normalise GspPostfix = Postfix
normalise o = o

flatten :: Tree -> DOrd -> [Int]
flatten Nil o = []
flatten (Node root left right) o
    | o `elem` [GspInfix, GspPrefix, GspPostfix] = flatten (Node root right left) (normalise o)
    | o == Infix = flatten left o ++ [root] ++ flatten right o
    | o == Prefix  = [root] ++ flatten left o ++ flatten right o
    | o == Postfix  = flatten left o ++ flatten right o ++ [root]
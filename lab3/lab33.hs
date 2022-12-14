-- prob 62B

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
leaf x = Branch x Empty Empty

atLevel :: Tree a -> Int -> [a]
atLevel Empty          _ = []
atLevel (Branch x _ _) 0 = [x]
atLevel (Branch _ a b) n = atLevel a n' ++ atLevel b n'
                           where n' = n - 1
                           
testtree = Branch 10
    (Branch 5 (Branch 3 (leaf 2) (leaf 4))
              (Branch 7 (Empty) (leaf 8)))
    (Branch 13 (leaf 12) (leaf 20))
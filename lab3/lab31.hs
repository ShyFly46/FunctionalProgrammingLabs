-- prob 55

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
leaf x = Branch x Empty Empty

cbaltree :: Int -> [Tree Char]
cbaltree 0 = [Empty]
cbaltree 1 = [leaf 'x']
cbalTree n = if n `mod` 2 == 1 then 
             [ Branch 'x' l r | l <- cbalTree ((n - 1) `div` 2), 
                                r <- cbalTree ((n - 1) `div` 2) ] 
             else 
             concat [ [Branch 'x' l r, Branch 'x' r l] | l <- cbalTree ((n - 1) `div` 2), 
                                                         r <- cbalTree (n `div` 2) ]
-- prob 1
-- я даже не представляю, как можно применить монады
-- в решении подобных задачек
-- поэтому вот...

return' :: [a] -> Maybe [a]
return' [] = Nothing
return' l  = Just l

last'' :: [a] -> Maybe a
last'' [x] = Just x
last'' (_:xs) = last'' xs

last' :: [a] -> Maybe a
last' xs = return' xs >>= \a -> last'' a

 
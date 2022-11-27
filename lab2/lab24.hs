--prob 20
myRemoveAt :: Int -> [a] -> (a, [a])
myRemoveAt 1 (x:xs) = (x, xs)
myRemoveAt i x:xs
           | i < 1 = error "Something went wrong"
           | otherwise = let (a,r) = myRemoveAt i-1 xs in (a, x:r)
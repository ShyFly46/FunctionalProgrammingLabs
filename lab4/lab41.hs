-- prob 15

multiplier :: Int -> a -> [a]
multiplier 0 _ = []
multiplier 1 x = [x]
multiplier n x = x : multiplier (n-1) x

listmapper [] _ = []
listmapper (x:xs) f = (f x) ++ (listmapper xs f)

repli ll n = listmapper ll (multiplier n)
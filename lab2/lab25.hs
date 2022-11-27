-- prob 21
myinsertat :: a -> [a] -> Int -> [a]
myinsertat x ys 1 = x:ys
myinsertat x (y:ys) i = y: myinsertat x ys i-1
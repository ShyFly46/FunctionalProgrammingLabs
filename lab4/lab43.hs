-- prob 4

myFold _ x0 [] = x0
myFold f x0 (x:xs) = myFold f (f x0 x) xs

myCounter x _ = x+1

myLength = myFold myCounter 0
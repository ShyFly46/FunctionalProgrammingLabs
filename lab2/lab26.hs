-- prob 22
myrange :: Int -> Int -> [Int]
myrange a b
       | a == b = [b]
       | a < b = a: myrange a+1 b
       | a > b = a: myrange a-1 b
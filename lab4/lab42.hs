-- prob 11

data List' a = Single a | Mult Int a
    deriving (Show)

group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) = let (first, rest) = span (==x) xs
               in (x:first) : group rest

encode :: [a] -> (Int, a)
encode x = (length x, head x)

encode1 :: (Int, a) -> List' a
encode1 (1, x) = Single x
encode1 (n, x) = Mult n x

encodeModified :: Eq a => [a] -> [List' a]
encodeModified xs = map (encode1 . encode) (group xs)
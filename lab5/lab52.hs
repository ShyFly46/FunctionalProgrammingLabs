-- prob 2
import System.IO

return' :: [a] -> Maybe [a]
return' [] = Nothing
return' l  = Just l

butlast'' :: [a] -> Maybe a
butlast'' [] = Nothing
butlast'' [_] = Nothing
butlast'' [x,_] = Just x
butlast'' (_:xs) = butlast'' xs

butlast' :: [a] -> Maybe a
butlast' xs = return' xs >>= \a -> butlast'' a

afs :: Maybe Char -> [Char]
afs Nothing = "empty\n"
afs (Just cc) = "found " ++ [cc] ++ "!\n"
    
main = do -- это "синтаксический сахар" монады
    handle <- openFile "in.txt" ReadMode
    contents <- hGetContents handle
    let ans = afs $ butlast' contents
    print ans
    writeFile "out.txt" ans
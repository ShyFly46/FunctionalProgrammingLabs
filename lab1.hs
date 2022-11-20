findRoots :: (Float, Float, Float) -> [Float]
findRoots (a, b, c)
    | d >  0 = let f = sqrt d / (2*a)
               in [e - f, e + f]
    | d == 0 = [e]
    | d <  0 = []
    where d = b*b - 4*a*c
          e = -b / 2*a

main = do
    putStrLn "a, b, c?"
    aa <- getLine
    bb <- getLine
    cc <- getLine
    let polyn = (read aa :: Float, read bb :: Float, read cc :: Float)
    putStr $ "roots = "
    print $ findRoots polyn
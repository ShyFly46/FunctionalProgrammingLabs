data MyRoots = RNo | ROne Float | RTwo Float Float deriving (Show)

findRoots :: (Float, Float, Float) -> MyRoots
findRoots (a, b, c)
    | d >  0 = let f = sqrt d / (2*a)
               in RTwo (e - f) (e + f)
    | d == 0 = ROne e
    | d <  0 = RNo
    where d = b*b - 4*a*c
          e = -b / 2*a

main = do
    aa <- getLine
    bb <- getLine
    cc <- getLine
    let polyn = (read aa :: Float, read bb :: Float, read cc :: Float)
    print $ findRoots polyn

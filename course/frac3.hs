import Graphics.Gloss

mywindow = InWindow "Nicer Tree" (600, 600) (20, 20)

brown = makeColorI 119 67 21 255 
 
mytree' :: Int -> Picture
mytree' 0 = Blank
mytree' n = Pictures [branch_m, branch_l, branch_r]
    where angle = 40
          colprop = fromIntegral (n-1) / 7.0 :: Float
          mycolor = mixColors colprop (1.0 - colprop) brown green
          myline = line [(0,   0),
                         (0, 100)]
          branch_m
              = color mycolor
              $ myline
          smallerbranch = mytree' (n-1)
          branch_l
              = translate 0 100
              $ scale 0.65 0.65
              $ rotate (20 - angle)
              $ smallerbranch
          branch_r
              = translate 0 100
              $ scale 0.5 0.5
              $ rotate angle
              $ smallerbranch
              
myfigure = mytree' 8
              
main = display mywindow white myfigure

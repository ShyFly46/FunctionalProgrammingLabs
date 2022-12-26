import Graphics.Gloss

mywindow = InWindow "Tree" (600, 600) (20, 20)

brown = makeColorI 119 67 21 255

mytree :: Int -> Picture
mytree 0 = Blank
mytree n = Pictures [branch_m, branch_l, branch_r]
    where angle = 45
          colprop = fromIntegral (n-1) / 12.0 :: Float
          mycolor = mixColors colprop (1.0 - colprop) brown green
          myline = line [(0,   0),
                         (0, 100)]
          branch_m
              = color mycolor
              $ myline
          smallerbranch
              = scale 0.6 0.6
              $ mytree (n-1)
          branch_l
              = translate 0 100
              $ rotate (-angle)
              $ smallerbranch
          branch_r
              = translate 0 100
              $ rotate angle
              $ smallerbranch
              
              
myfigure = mytree 8
              
main = display mywindow white myfigure

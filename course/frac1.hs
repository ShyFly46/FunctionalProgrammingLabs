import Graphics.Gloss

mywindow = InWindow "Circles" (600, 600) (20, 20)

mycirc :: Int -> Picture
mycirc 0 = Blank
mycirc n = Pictures [circ_c, circ_l, circ_r]
    where x = 200
          circ_c = color black $ circle x
          circ_small
              = color black
              $ scale 0.45 0.45
              $ mycirc (n-1)
          circ_r = translate   x  0 $ circ_small
          circ_l = translate (-x) 0 $ circ_small

myfigure = Pictures [line [(-1000,0),(1000,0)], mycirc 6]

main = display mywindow white myfigure

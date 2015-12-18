import Types
import Graphics.GL.Pal

someCubes1 :: Float -> [Cube]
someCubes1 t =
    [ newCube
        { cubeColor    = hslColor (x*0.01+t*0.3) 0.9 0.4 1 
        , cubeRotation = axisAngle (V3 0 1 0) 2
        , cubePosition = V3 (sin (t+x*0.11)) 
                            (x*0.1-1) 
                            0
        , cubeScale    = V3 (0.1*x + 0.5)
                            0.1
                            (0.1)
        } 
      | x <- [0..n]
    ]
    where n = fromIntegral $ min 100 (mod (floor (t*50)) 100)

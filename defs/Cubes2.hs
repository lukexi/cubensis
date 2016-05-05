import Types
import Graphics.GL.Pal

someCubes2 :: Float -> [Cube]
someCubes2 = \t ->
    [ newCube
        { cubeColor    = colorHSL (x*0.01+t*0.3) 0.8 0.4
        , cubeRotation = axisAngle (V3 0 1 0) (t*1)
        , cubePosition = V3 (sin (t+x*0.11) * 0.1)
                            (x*0.1-1.5)
                            4
        , cubeScale    = V3 (x*0.01) 0.1 0.1
        }
      | x <- [0..50]
    ]

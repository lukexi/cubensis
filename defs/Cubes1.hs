import Types
import Graphics.GL.Pal

someCubes :: Float -> [Cube]
someCubes = \t -> 
    [ newCube
        { cubeColor    = hslColor (x*0.01+t*0.3) 0.8 0.4 1 
        , cubeRotation = axisAngle (V3 0 1 0) (t*1)
        , cubePosition = V3 (sin (t+x*0.11)*10) 
                            (x*0.1-1.5) 
                            (-11 + sin (t + x))
        , cubeScale    = V3 (0.1 + realToFrac (sin t) * 0.1) 
                            0.5
                            (sin t*5+4)
        } 
      | x <- [0..500]
    ]

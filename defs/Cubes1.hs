import Types
import Graphics.GL.Pal

someCubes :: Float -> [Cube]
someCubes = \t -> 
    [ newCube
        { cubeColor    = hslColor (x*0.01+t*0.3) 0.8 0.4 1 
        , cubePosition = V3 (sin (t+x*0.11)) (x*0.1-1.5) (-11)
        , cubeRotation = axisAngle (V3 0 0.5 0.5) t
        , cubeScale    = V3 (0.05 + realToFrac (sin t) * 0.1) 0.1 (sin t*5)
        } 
      | x <- [0..350]
    ]

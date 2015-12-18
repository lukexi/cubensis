import Types
import Graphics.GL.Pal

someCubes :: Float -> [Cube]
someCubes = \t -> 
    [ newCube
        { cubeColor = hslColor (x+z+t*1) 0.8 0.4 1 
        , cubePosition = V3 (x*3) (sin (z + t*0.5) + sin (z + t)) (sin t * z)
        , cubeRotation = axisAngle (V3 0 0.5 0.5) (t*2)
        , cubeScale = (V3 (0.11 + realToFrac (sin t) * 0.1) 0.33 (sin t))
        } 
      | x <- [-5, -4.8..5]
      , z <- [-5, -4.8..0]
    ]

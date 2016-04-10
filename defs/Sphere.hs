 import Types
import Graphics.GL.Pal

sphere :: Float -> [Cube]
sphere t =
    [ newCube
        { cubeColor    = hslColor (x*0.1+t*0.5) 0.8 0.6 
        , cubeRotation = axisAngle (V3 0 0 1) t
        , cubePosition = rotate (axisAngle (V3 0 1 0) (x*0.2)) 
                       . rotate (axisAngle (V3 1 0 0) (x*0.01)) 
                       $ (V3 0 1 1)
        , cubeScale    = V3
            (realToFrac $ 0.1  * sin (x+t*5))
            (realToFrac $ 0.15 * sin (x+t*4))
            (realToFrac $ 0.2  * sin (x+t*6))
        } 
      | x <- [0..n]
    ]
    where n = fromIntegral $ abs $ (subtract (maxx `div` 2)) $ min maxx (mod (floor (t*50)) maxx)
          maxx = 6000
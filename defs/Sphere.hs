import Types
import Graphics.GL.Pal

sphere :: Float -> [Cube]
sphere t =
    [ newCube
        { cubeColor    = hslColor (x*0.1+t*0.5) 0.8 0.6 
        , cubeRotation = axisAngle (V3 0 0 1) t
        , cubePosition = rotate (axisAngle (V3 0 1 0) (x*0.3)) 
                       . rotate (axisAngle (V3 1 0 0) (x*0.01)) 
                       $ (V3 0 1 1)
        , cubeScale    = 0.1
        } 
      | x <- [0..n]
    ]
    where n = fromIntegral $ abs $ (subtract (maxx `div` 2)) $ min maxx (mod (floor (t*50)) maxx)
          maxx = 600
import Types
import Graphics.GL.Pal

sphere :: Float -> [Cube]
sphere t =
    [ newCube
        { cubeColor    = hslColor (x*0.01+t*0.3) 0.9 0.4 1 
        , cubeRotation = axisAngle (V3 0 0 1) t
        , cubePosition = rotate (axisAngle (V3 1 0 0) (x*0.03)) 
                       . rotate (axisAngle (V3 0 1 0) (x*0.5)) 
                       $ (V3 0 0 1)
        , cubeScale    = 0.1
        } 
      | x <- [0..n]
    ]
    where n = fromIntegral $ min maxx (mod (floor (t*50)) maxx)
          maxx = 500

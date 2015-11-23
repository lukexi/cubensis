module Main where

import Cubensis



main :: IO ()
main = do
  cubensis $ \t -> 
    [ newCube 
        
        { cubeColor = hslColor (x+z+t) 0.8 0.7 1 
        , cubePosition = V3 x (sin (x + t) + sin (z + t)) z
        , cubeRotation = axisAngle (V3 1 1 0) t
        , cubeScale = 0.11 + realToFrac (sin t) * 0.1
        } 
      | x <- [-5, -4.8..5]
      , z <- [-5, -4.8..0]
    ]

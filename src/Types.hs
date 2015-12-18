module Types where
import Graphics.GL.Pal

data Cube = Cube
  { cubeColor :: V4 GLfloat
  , cubeScale :: V3 GLfloat
  , cubePosition :: V3 GLfloat
  , cubeRotation :: Quaternion GLfloat
  } deriving Show

newCube :: Cube
newCube = Cube
  { cubeColor    = V4 1 1 1 1
  , cubeScale    = V3 1 1 1
  , cubePosition = V3 0 0 0
  , cubeRotation = axisAngle (V3 0 1 0) 0
  }

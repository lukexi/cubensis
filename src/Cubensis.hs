{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Cubensis 
  ( module Cubensis
  , module Exports
  ) where

import Graphics.UI.GLFW.Pal as Exports
import Graphics.VR.Pal as Exports
import Graphics.GL.Pal as Exports

import Data.Time
import Control.Monad
import Control.Monad.Reader
import Halive.Utils

data Cube = Cube
  { cubeColor :: V4 GLfloat
  , cubeScale :: V3 GLfloat
  , cubePosition :: V3 GLfloat
  , cubeRotation :: Quaternion GLfloat
  }

newCube :: Cube
newCube = Cube
  { cubeColor    = V4 1 1 1 1
  , cubeScale    = V3 1 1 1
  , cubePosition = V3 0 0 0
  , cubeRotation = axisAngle (V3 0 1 0) 0
  }

data Uniforms = Uniforms 
  { uMVP   :: UniformLocation (M44 GLfloat) 
  , uModel :: UniformLocation (M44 GLfloat) 
  , uColor :: UniformLocation (V4 GLfloat) 
  } deriving Data

cubensis :: (Fractional a) => (a -> [Cube]) -> IO ()
cubensis getPositions = do
  vrPal@VRPal{..} <- reacquire 0 $ initVRPal "Cubensis" [UseOpenVR]

  shader        <- createShaderProgram "src/geo.vert" "src/geo.frag"
  uniforms      <- acquireUniforms shader

  cubeGeo       <- cubeGeometry 1 5
  cubeShape     <- (makeShape cubeGeo shader :: IO (Shape Uniforms))

  glEnable GL_DEPTH_TEST
  glClearColor 0.0 0.0 0.1 1

  -- let viewMat = viewMatrixFromPose newPose
  let view44 = viewMatrix (V3 0 0 5) (axisAngle (V3 0 1 0) 0)

  whileWindow gpWindow $ do
    processEvents gpEvents $ closeOnEscape gpWindow

    now <- realToFrac . utctDayTime <$> getCurrentTime

    renderWith vrPal view44 
      (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
      $ \proj44 eyeView44 -> do
          let projView = proj44 !*! eyeView44
          withShape cubeShape $ 
            render (uniforms :: Uniforms) projView (getPositions now)

render :: (MonadIO m, MonadReader (Shape u) m) 
       => Uniforms -> M44 GLfloat -> [Cube] -> m ()
render Uniforms{..} projView cubes = do
  forM_ cubes $ \Cube{..} -> do
    let model = mkTransformation cubeRotation cubePosition !*! scaleMatrix cubeScale
    uniformV4 uColor cubeColor
    uniformM44 uMVP   (projView !*! model)
    uniformM44 uModel model
    drawShape

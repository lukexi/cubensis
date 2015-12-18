{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Cubensis where

import Graphics.UI.GLFW.Pal as Exports
import Graphics.VR.Pal as Exports
import Graphics.GL.Pal as Exports
import TinyRick

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent
import Halive.Utils
import SubHalive
import Data.Maybe

import Types

data Uniforms = Uniforms 
  { uMVP   :: UniformLocation (M44 GLfloat) 
  , uModel :: UniformLocation (M44 GLfloat) 
  , uColor :: UniformLocation (V4 GLfloat) 
  } deriving Data


main :: IO ()
main = do
  vrPal@VRPal{..} <- reacquire 0 $ initVRPal "Cubensis" [UseOpenVR]

  shader        <- createShaderProgram "shaders/geo.vert" "shaders/geo.frag"
  uniforms      <- acquireUniforms shader

  cubeGeo       <- cubeGeometry 1 5
  cubeShape     <- (makeShape cubeGeo shader :: IO (Shape Uniforms))

  glyphProg     <- createShaderProgram "shaders/glyph.vert" "shaders/glyph.frag"
  font          <- createFont "fonts/SourceCodePro-Regular.ttf" 50 glyphProg

  textMVar <- reacquire 1 $ newMVar =<< textRendererFromFile font "defs/Cubes1.hs"

  funcMVar <- recompilerForExpression "defs/Cubes1.hs" "someCubes" (const [])

  glEnable GL_DEPTH_TEST
  glEnable GL_BLEND
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
  glClearColor 0.0 0.0 0.1 1

  let player  = Pose (V3 0 0 5) (axisAngle (V3 0 1 0) 0)
      textM44 = mkTransformation (axisAngle (V3 1 0 0) 0) (V3 0 0 4)

  start <- getNow

  

  whileVR vrPal $ \headM44 _hands -> do
    player' <- execStateT (applyMouseLook gpWindow id) player

    handleEvents vrPal textMVar textM44 player'

    -- Have time start at 0 seconds
    now <- (subtract start) <$> getNow
    let _ = now::Float

    text <- readMVar textMVar
    func <- readMVar funcMVar
    let cubes = func now
    renderWith vrPal player' headM44
      (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
      $ \proj44 eyeView44 -> do
          let projViewM44 = proj44 !*! eyeView44
          withShape cubeShape $ 
            renderCubes (uniforms :: Uniforms) projViewM44 cubes
          let textMVP = projViewM44 !*! textM44
          renderText text textMVP (V3 1 1 1)


handleEvents :: VRPal -> MVar TextRenderer -> M44 Float -> Pose Float -> IO ()
handleEvents VRPal{..} textMVar textModelM44 playerPose = processEvents gpEvents $ \e -> do
  closeOnEscape gpWindow e

  modifyMVar textMVar $ \text -> do 
    newText <- execStateT (handleTextBufferEvent gpWindow e id) text
    return (newText, ())

  onMouseDown e $ \_ -> modifyMVar textMVar $ \text -> do
    winProj44 <- getWindowProjection gpWindow 45 0.1 1000
    ray       <- cursorPosToWorldRay gpWindow winProj44 playerPose
    newText   <- castRayToBuffer ray text textModelM44
    return (newText, ())

renderCubes :: (MonadIO m, MonadReader (Shape u) m) 
       => Uniforms -> M44 GLfloat -> [Cube] -> m ()
renderCubes Uniforms{..} projView cubes = do
  forM_ cubes $ \Cube{..} -> do
    let model = mkTransformation cubeRotation cubePosition !*! scaleMatrix cubeScale
    uniformV4  uColor cubeColor
    uniformM44 uMVP   (projView !*! model)
    uniformM44 uModel model
    drawShape

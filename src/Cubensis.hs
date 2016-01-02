{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Cubensis where

import Graphics.UI.GLFW.Pal as Exports
import Graphics.VR.Pal as Exports
import Graphics.GL.Pal as Exports
import TinyRick

import Control.Monad
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent
import Control.Exception
import Control.DeepSeq

import Halive.Utils
import SubHalive

import Types

data Uniforms = Uniforms 
  { uMVP   :: UniformLocation (M44 GLfloat) 
  , uModel :: UniformLocation (M44 GLfloat) 
  , uColor :: UniformLocation (V4 GLfloat) 
  } deriving Data


data Editor a = Editor
  { edText     :: MVar TextRenderer
  , edExpr     :: MVar (a, [String])
  , edModelM44 :: M44 GLfloat
  }

makeExpressionEditor :: Chan (FilePath, String, MVar (b -> [t], [String]))
                     -> Font
                     -> FilePath
                     -> String
                     -> M44 GLfloat
                     -> IO (Editor (b -> [t]))
makeExpressionEditor ghcChan font fileName expr modelM44 = do
  textMVar <- newMVar =<< textRendererFromFile font fileName
  funcMVar <- recompilerForExpression ghcChan fileName expr (const [])

  return (Editor textMVar funcMVar modelM44)

-- Negative == Clockwise
textTilt :: Float
textTilt = -0.1 * 2 * pi

textM44 :: M44 Float
textM44 = mkTransformation (axisAngle (V3 1 0 0) textTilt) (V3 0 (-1) 4)

player :: Pose Float
player  = Pose (V3 0 0 5) (axisAngle (V3 0 1 0) 0)

main :: IO ()
main = do
  ghcChan <- startGHC ["defs"]

  vrPal@VRPal{..} <- reacquire 0 $ initVRPal "Cubensis" [UseOpenVR]

  glEnable GL_DEPTH_TEST
  glEnable GL_BLEND
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
  glClearColor 0.0 0.0 0.1 1

  shader        <- createShaderProgram "shaders/geo.vert" "shaders/geo.frag"

  cubeGeo       <- cubeGeometry 1 5
  cubeShape     <- (makeShape cubeGeo shader :: IO (Shape Uniforms))

  glyphProg     <- createShaderProgram "shaders/glyph.vert" "shaders/glyph.frag"
  font          <- createFont "fonts/SourceCodePro-Regular.ttf" 50 glyphProg

  editors <- sequence 
    [ makeExpressionEditor ghcChan font "defs/Sphere.hs" "sphere" (identity & translation .~ V3 0 0 0)
    -- , makeExpressionEditor ghcChan font "defs/Cubes1.hs" "someCubes1" (identity & translation .~ V3 0 0 0)
    -- , makeExpressionEditor ghcChan font "defs/Cubes2.hs" "someCubes2" (identity & translation .~ V3 3 0 0)
    ]
  
  start <- getNow

  whileVR vrPal $ \headM44 _hands -> do
    player' <- execStateT (applyMouseLook gpWindow id) player

    let activeEditor = head editors
    handleEvents vrPal player' activeEditor

    -- Have time start at 0 seconds
    now <- (subtract start) <$> getNow
    let _ = now::Float

    renderWith vrPal player' headM44
      (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
      $ \proj44 eyeView44 -> do
          let projViewM44 = proj44 !*! eyeView44

          forM_ editors $ \Editor{..} -> do
            (func, errors) <- readMVar edExpr
            text <- readMVar edText
            let cubes = take 10000 (func now) -- Allow the function to try to return infinite cubes, but don't use them all
            (cubes', runtimeErrors) <- handle (\e -> return ([], show (e::SomeException))) (return $!! (cubes, ""))
            let errors' = runtimeErrors:errors
            withShape cubeShape $ 
              renderCubes projViewM44 edModelM44 cubes'

            let textMVP = projViewM44 !*! edModelM44 !*! textM44
            renderText text textMVP (V3 1 1 1)

            let errorsMVP = projViewM44 
                            !*! edModelM44 
                            !*! (identity & translation .~ V3 1 0 0)
                            !*! textM44
            errorRenderer <- createTextRenderer font (textBufferFromString "noFile" (unlines errors'))
            renderText errorRenderer errorsMVP (V3 1 0.5 0.5)


renderCubes :: (MonadIO m, MonadReader (Shape Uniforms) m) 
            => M44 GLfloat -> M44 GLfloat -> [Cube] -> m ()
renderCubes projViewM44 parentModelM44 cubes = do
  Uniforms{..} <- asks sUniforms
  forM_ cubes $ \Cube{..} -> do
    let modelM44 = parentModelM44 
                   !*! mkTransformation cubeRotation cubePosition
                   !*! scaleMatrix cubeScale
    uniformV4  uColor cubeColor
    uniformM44 uMVP   (projViewM44 !*! modelM44)
    uniformM44 uModel modelM44
    drawShape


handleEvents :: VRPal -> Pose Float -> Editor a -> IO ()
handleEvents VRPal{..} playerPose Editor{..} = processEvents gpEvents $ \e -> do
  closeOnEscape gpWindow e

  modifyMVar edText $ \text -> do 
    newText <- execStateT (handleTextBufferEvent gpWindow e id) text
    return (newText, ())

  onMouseDown e $ \_ -> modifyMVar edText $ \text -> do
    winProj44 <- getWindowProjection gpWindow 45 0.1 1000
    ray       <- cursorPosToWorldRay gpWindow winProj44 playerPose
    newText   <- castRayToBuffer ray text (edModelM44 !*! textM44)
    return (newText, ())

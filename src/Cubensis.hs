{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Cubensis where

import Graphics.UI.GLFW.Pal
import Graphics.VR.Pal
import Graphics.GL.Pal hiding (getNow)

import Control.Monad
import Control.Lens.Extra
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.DeepSeq
import Data.Time

import Halive.Utils
import Halive.SubHalive
import Halive.Recompiler
import Halive.FileListener
import Graphics.GL.Freetype
import Graphics.GL.TextBuffer

import Types

data Uniforms = Uniforms 
  { uMVP   :: UniformLocation (M44 GLfloat) 
  , uModel :: UniformLocation (M44 GLfloat) 
  , uColor :: UniformLocation (V4 GLfloat) 
  } deriving Data

data World = World
    { _wldPlayer :: M44 GLfloat
    , _wldEditor :: Editor
    }

data Editor = Editor
    { _edTextRenderer :: TextRenderer
    , _edValue        :: Float -> [Cube]
    , _edRecompiler   :: Recompiler
    , _edModelM44     :: M44 GLfloat
    }
makeLenses ''Editor
makeLenses ''World

makeExpressionEditor ghcChan font fileName exprName modelM44 = do
    resultsChan  <- recompilerForExpression ghcChan fileName exprName
    textRenderer <- textRendererFromFile font fileName WatchFile

    return $ Editor textRenderer (const []) resultsChan modelM44

tickEditor now = do

    editor <- use wldEditor
    
    refreshTextRendererFromFile (wldEditor . edTextRenderer)

    result <- liftIO . atomically $ tryReadTChan (recResultTChan $ editor ^. edRecompiler)
    
    case result of
        Just (Right compiledValue) -> wldEditor . edValue .= getCompiledValue compiledValue
        Just (Left errors)         -> liftIO . putStrLn . concat $ errors
        _ -> return ()

    func <- use $ wldEditor . edValue

    return $ take 10000 (func now)



-- Negative == Clockwise
textTilt :: GLfloat
textTilt = -0.1 * 2 * pi

textM44 :: M44 GLfloat
textM44 = mkTransformation (axisAngle (V3 1 0 0) textTilt) (V3 0 (-1) 4) !*! scaleMatrix (1/50)

playerStart :: M44 GLfloat
playerStart  = mkTransformation (axisAngle (V3 0 1 0) 0) (V3 0 0 5)

main :: IO ()
main = do
    ghcChan <- startGHC defaultGHCSessionConfig

    vrPal@VRPal{..} <- reacquire 0 $ initVRPal "Cubensis" [UseOpenVR]

    glEnable GL_DEPTH_TEST
    
    glClearColor 0.0 0.0 0.1 1

    shader        <- createShaderProgram "shaders/geo.vert" "shaders/geo.frag"

    cubeGeo       <- cubeGeometry 1 5
    cubeShape     <- (makeShape cubeGeo shader :: IO (Shape Uniforms))

    glyphProg     <- createShaderProgram "shaders/glyph.vert" "shaders/glyph.frag"
    font          <- createFont "fonts/SourceCodePro-Regular.ttf" 50 glyphProg

    editor        <- makeExpressionEditor ghcChan font "defs/Sphere.hs" "sphere" (identity & translation .~ V3 0 0 0)
    
    start <- realToFrac . utctDayTime <$> getNow vrPal

    let world = World playerStart editor
    void . flip runStateT world . whileWindow gpWindow $ do
        --applyMouseLook gpWindow wldPlayer
        player <- use wldPlayer

        (headM44, events) <- tickVR vrPal player
        handleEvents vrPal events

        -- Have time start at 0 seconds
        now <- subtract start . realToFrac . utctDayTime <$> getNow vrPal
        let _ = now::Float


        editor <- use wldEditor
        let modelM44 = editor ^. edModelM44
        -- Allow the function to try to return infinite cubes, but don't use them all

        cubes <- tickEditor now
        --cubes <- getEditorValue editor [] (\func -> take 10000 (func now))

        renderWith vrPal headM44 $ \proj44 eyeView44 -> do
            glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
            
            let projViewM44 = proj44 !*! eyeView44
                
            withShape cubeShape $ 
                renderCubes projViewM44 modelM44 cubes
            glEnable GL_BLEND
            glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
            renderText (editor ^. edTextRenderer) projViewM44 (modelM44 !*! textM44)
            glDisable GL_BLEND

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

--handleEvents :: (MonadState World m, MonadIO m) => VRPal -> m ()
handleEvents VRPal{..} events = forM_ events $ \case
    VREvent _ -> return ()
    GLFWEvent e -> do
        closeOnEscape gpWindow e

        editor          <- use wldEditor
        
        let isKey = case e of
                Key _ _ _ _ -> True
                Character _ -> True
                _           -> False
        when isKey $ do 
            setIgnoreTimeNow (editor ^. edRecompiler . to recFileEventListener)
            forM_ (editor ^. edTextRenderer . txrFileEventListener) setIgnoreTimeNow

        handleTextBufferEvent gpWindow e (wldEditor . edTextRenderer)

        player <- use wldPlayer
        onMouseDown e $ \_ -> do
            winProj44       <- getWindowProjection gpWindow 45 0.1 1000
            ray             <- cursorPosToWorldRay gpWindow winProj44 (poseFromMatrix player)
            newTextRenderer <- setCursorTextRendererWithRay ray (editor ^. edTextRenderer) (editor ^. edModelM44 !*! textM44)
            wldEditor . edTextRenderer .= newTextRenderer

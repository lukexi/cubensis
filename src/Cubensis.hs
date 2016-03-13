{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Cubensis where

import Graphics.UI.GLFW.Pal as Exports
import Graphics.VR.Pal as Exports
import Graphics.GL.Pal as Exports

import Control.Monad
import Control.Lens.Extra
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.DeepSeq

import Halive.Utils
import Halive.SubHalive
import Halive.Recompiler
import Graphics.GL.Freetype
import Graphics.GL.TextBuffer

import Types

data Uniforms = Uniforms 
  { uMVP   :: UniformLocation (M44 GLfloat) 
  , uModel :: UniformLocation (M44 GLfloat) 
  , uColor :: UniformLocation (V4 GLfloat) 
  } deriving Data

data World = World
    { _wldPlayer :: Pose GLfloat
    , _wldEditor :: Editor
    }

data Editor = Editor
    { _edTextRenderer :: TextRenderer
    , _edValue        :: Float -> [Cube]
    , _edResultsChan  :: TChan CompilationResult
    , _edModelM44     :: M44 GLfloat
    }
makeLenses ''Editor
makeLenses ''World

makeExpressionEditor ghcChan font fileName exprName modelM44 = do
    resultsChan  <- recompilerForExpression ghcChan fileName exprName
    textRenderer <- textRendererFromFile font fileName

    return $ Editor textRenderer (const []) resultsChan modelM44

tickEditor now = do
    editor <- use wldEditor
    result <- tryReadTChanIO (editor ^. edResultsChan)
    
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
textM44 = mkTransformation (axisAngle (V3 1 0 0) textTilt) (V3 0 (-1) 4)

playerStart :: Pose GLfloat
playerStart  = Pose (V3 0 0 5) (axisAngle (V3 0 1 0) 0)

main :: IO ()
main = do
    ghcChan <- startGHC ["defs"]

    vrPal@VRPal{..} <- reacquire 0 $ initVRPal "Cubensis" [UseOpenVR]

    glEnable GL_DEPTH_TEST
    
    glClearColor 0.0 0.0 0.1 1

    shader        <- createShaderProgram "shaders/geo.vert" "shaders/geo.frag"

    cubeGeo       <- cubeGeometry 1 5
    cubeShape     <- (makeShape cubeGeo shader :: IO (Shape Uniforms))

    glyphProg     <- createShaderProgram "shaders/glyph.vert" "shaders/glyph.frag"
    font          <- createFont "fonts/SourceCodePro-Regular.ttf" 50 glyphProg

    editor        <- makeExpressionEditor ghcChan font "defs/Sphere.hs" "sphere" (identity & translation .~ V3 0 0 0)
    
    start <- getNow

    let world = World playerStart editor
    void . flip runStateT world . whileVR vrPal $ \headM44 _hands events -> do
        handleEvents vrPal
        applyMouseLook gpWindow wldPlayer
        player <- use wldPlayer

        -- Have time start at 0 seconds
        now <- subtract start <$> getNow
        let _ = now::Float


        editor <- use wldEditor
        let modelM44 = editor ^. edModelM44
        -- Allow the function to try to return infinite cubes, but don't use them all

        cubes <- tickEditor now
        --cubes <- getEditorValue editor [] (\func -> take 10000 (func now))

        renderWith vrPal player headM44
            (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
            $ \proj44 eyeView44 -> do
                
                let projViewM44 = proj44 !*! eyeView44
                    
                withShape cubeShape $ 
                    renderCubes projViewM44 modelM44 cubes
                glEnable GL_BLEND
                glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
                renderText (editor ^. edTextRenderer) (projViewM44 !*! modelM44 !*! textM44) (V3 1 1 1)
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
handleEvents VRPal{..} = processEvents gpEvents $ \e -> do
    closeOnEscape gpWindow e

    handleTextBufferEvent gpWindow e (wldEditor . edTextRenderer)

    player <- use wldPlayer
    onMouseDown e $ \_ -> do
        editor          <- use wldEditor
        winProj44       <- getWindowProjection gpWindow 45 0.1 1000
        ray             <- cursorPosToWorldRay gpWindow winProj44 player
        newTextRenderer <- setCursorTextRendererWithRay ray (editor ^. edTextRenderer) (editor ^. edModelM44 !*! textM44)
        wldEditor . edTextRenderer .= newTextRenderer

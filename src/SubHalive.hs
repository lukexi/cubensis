{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables #-}
module SubHalive where

import GHC
import Linker
import Packages
import DynFlags
import Exception
import GHC.Paths
import Outputable
import Unsafe.Coerce

import Control.Monad
import Control.Monad.IO.Class

import Control.Concurrent
import FindPackageDBs

import System.FSNotify
import System.FilePath

fileModifiedPredicate :: FilePath -> Event -> Bool
fileModifiedPredicate fileName event = case event of
    -- Modified path _ -> path == fileName
    Modified path _ -> True
    _               -> False

eventListenerForFile :: FilePath -> IO (Chan Event)
eventListenerForFile fileName = do
    let predicate = fileModifiedPredicate fileName
    eventChan <- newChan
    _ <- forkIO . withManager $ \manager -> do
        let watchDirec = "."
        _stop <- watchTreeChan manager watchDirec predicate eventChan
        forever (threadDelay 10000000)
    return eventChan

recompilerForExpression :: FilePath -> String -> a -> IO (MVar a)
recompilerForExpression fileName expression defaultValue = do

    valueMVar <- newMVar defaultValue

    listenerChan <- eventListenerForFile fileName
  
    _ <- forkOS . void . withGHCSession fileName [] $ do
        let recompile = do
                result <- recompileTargets expression
                case result of
                  Just validResult -> liftIO (swapMVar valueMVar validResult) >> return ()
                  Nothing          -> return ()
        recompile
        forever $ do
            _ <- liftIO (readChan listenerChan)
            recompile
    return valueMVar

-- Starts up a GHC session and then runs the given action within it
withGHCSession :: FilePath -> [FilePath] -> Ghc (Maybe a) -> IO (Maybe a)
withGHCSession mainFileName extraImportPaths action = do
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ runGhc (Just libdir) $ do
        -- Add the main file's path to the import path list
        let mainFilePath   = dropFileName mainFileName
            allImportPaths = mainFilePath:extraImportPaths

        -- Get the default dynFlags
        dflags0 <- getSessionDynFlags
        
        -- If there's a sandbox, add its package DB
        dflags1 <- updateDynFlagsWithCabalSandbox dflags0

        -- If this is a stack project, add its package DBs
        dflags2 <- updateDynFlagsWithStackDB dflags1

        -- Make sure we're configured for live-reload, and turn off the GHCi sandbox
        -- since it breaks OpenGL/GUI usage
        let dflags3 = dflags2 { hscTarget   = HscInterpreted
                              , ghcLink     = LinkInMemory
                              , ghcMode     = CompManager
                              , importPaths = allImportPaths
                              } `gopt_unset` Opt_GhciSandbox
        
        -- We must call setSessionDynFlags before calling initPackages or any other GHC API
        _ <- setSessionDynFlags dflags3

        -- Initialize the package database
        (dflags4, _) <- liftIO (initPackages dflags3)

        -- Initialize the dynamic linker
        liftIO (initDynLinker dflags4)

        -- Set the given filename as a compilation target
        setTargets =<< sequence [guessTarget mainFileName Nothing]

        action

-- Recompiles the current targets
recompileTargets :: String -> Ghc (Maybe a)
recompileTargets expression = catchExceptions . handleSourceError (\e -> printException e >> return Nothing) $ do

    -- Get the dependencies of the main target
    graph <- depanal [] False

    -- Reload the main target
    loadSuccess <- load LoadAllTargets

    if failed loadSuccess 
        then 
            return Nothing
        else do
            -- We must parse and typecheck modules before they'll be available for usage
            forM_ graph (typecheckModule <=< parseModule)
            
            -- Load the dependencies of the main target
            setContext (IIModule . ms_mod_name <$> graph)

            result <- compileExpr expression

            return (Just . unsafeCoerce $ result)

typecheckTargets :: GhcMonad m => m () -> m ()
typecheckTargets onFailure = handleSourceError (\e -> onFailure >> printException e) $ do
    -- Get the dependencies of the main target
    graph <- depanal [] False

    -- Reload the main target
    loadSuccess <- load LoadAllTargets
    if failed loadSuccess 
        then onFailure
        else 
            -- Parse and typecheck modules to trigger any SourceErrors therein
            forM_ graph (typecheckModule <=< parseModule)

catchExceptions :: ExceptionMonad m => m (Maybe a) -> m (Maybe a)
catchExceptions a = gcatch a 
    (\(_x :: SomeException) -> return Nothing)

-- A helper from interactive-diagrams to print out GHC API values, 
-- useful while debugging the API.
-- | Outputs any value that can be pretty-printed using the default style
output :: (GhcMonad m, MonadIO m) => Outputable a => a -> m ()
output a = do
    dfs <- getSessionDynFlags
    let style = defaultUserStyle
    let cntx  = initSDocContext dfs style
    liftIO $ print $ runSDoc (ppr a) cntx

{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, Rank2Types, RecordWildCards #-}
module Main
    ( main
    ) where

import qualified Control.Exception as E
import           Control.Lens.Operators
import           Control.Monad (join, unless, replicateM_)
import           Data.IORef
import           Data.MRUMemo (memoIO)
import           Data.Maybe
import qualified Data.Monoid as Monoid
import           Data.Store.Db (Db)
import qualified Data.Store.IRef as IRef
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Data.Time.Clock (getCurrentTime)
import           GHC.Conc (setNumCapabilities, getNumProcessors)
import           Graphics.DrawingCombinators (Font)
import           Graphics.UI.Bottle.MainLoop (mainLoopWidget)
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.EventMapDoc as EventMapDoc
import qualified Graphics.UI.Bottle.Widgets.FlyNav as FlyNav
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFWUtils
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Sampler (Sampler)
import qualified Lamdu.Config.Sampler as ConfigSampler
import qualified Lamdu.Data.DbLayout as DbLayout
import qualified Lamdu.Data.ExampleDB as ExampleDB
import           Lamdu.DataFile (getLamduDir)
import qualified Lamdu.EvalManager as EvalManager
import qualified Lamdu.Font as Font
import           Lamdu.GUI.CodeEdit.Settings (Settings(..))
import qualified Lamdu.GUI.CodeEdit.Settings as Settings
import qualified Lamdu.GUI.Main as GUIMain
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.GUI.Zoom (Zoom)
import qualified Lamdu.GUI.Zoom as Zoom
import qualified Lamdu.Opts as Opts
import qualified Lamdu.Style as Style
import qualified Lamdu.VersionControl as VersionControl
import           Lamdu.VersionControl.Actions (mUndo)
import qualified System.Directory as Directory
import           System.IO (hPutStrLn, stderr)

import           Prelude.Compat

main :: IO ()
main =
    do
        setNumCapabilities =<< getNumProcessors
        lamduDir <- getLamduDir
        opts <- either fail return =<< Opts.get
        let withDB = ExampleDB.withDB lamduDir
        case opts of
            Opts.Parsed{..}
                | _poShouldDeleteDB -> deleteDB lamduDir
                | _poUndoCount > 0  -> withDB $ undoN _poUndoCount
                | otherwise         -> withDB $ runEditor _poMFontPath _poWindowMode
    `E.catch` \e@E.SomeException{} -> do
    hPutStrLn stderr $ "Main exiting due to exception: " ++ show e
    return ()

deleteDB :: FilePath -> IO ()
deleteDB lamduDir =
    do
        putStrLn "Deleting DB..."
        Directory.removeDirectoryRecursive lamduDir

undoN :: Int -> Db -> IO ()
undoN n db =
    do
        putStrLn $ "Undoing " ++ show n ++ " times"
        DbLayout.runDbTransaction db $ replicateM_ n undo
    where
        undo =
            do
                actions <- VersionControl.makeActions
                fromMaybe (fail "Cannot undo any further") $ mUndo actions

createWindow :: Opts.WindowMode -> IO GLFW.Window
createWindow Opts.VideoModeSize =
    GLFWUtils.getVideoModeSize >>=
    GLFWUtils.createWindow "Lamdu" Nothing
createWindow (Opts.WindowSize winSize) =
    GLFWUtils.createWindow "Lamdu" Nothing winSize
createWindow Opts.FullScreen =
    do
        mMonitor <- GLFW.getPrimaryMonitor
        GLFWUtils.getVideoModeSize >>= GLFWUtils.createWindow "Lamdu" mMonitor

settingsChangeHandler :: EvalManager.Evaluators -> Settings -> IO ()
settingsChangeHandler evaluators settings =
    case settings ^. Settings.sInfoMode of
    Settings.Evaluation -> EvalManager.start evaluators
    _ -> EvalManager.stop evaluators

makeRootWidget ::
    Font -> Db -> Zoom -> IORef Settings -> EvalManager.Evaluators ->
    (Config, Widget.Size) -> IO (Widget IO)
makeRootWidget font db zoom settingsRef evaluators (config, size) =
    do
        cursor <-
            DbLayout.cursor DbLayout.revisionProps
            & Transaction.getP
            & DbLayout.runDbTransaction db
        sizeFactor <- Zoom.getSizeFactor zoom
        globalEventMap <- Settings.mkEventMap (settingsChangeHandler evaluators) config settingsRef
        let eventMap = globalEventMap `mappend` Zoom.eventMap zoom (Config.zoom config)
        evalResults <- EvalManager.getResults evaluators
        settings <- readIORef settingsRef
        let env = GUIMain.Env
                { envEvalRes = evalResults
                , envConfig = config
                , envSettings = settings
                , envStyle = Style.base config font
                , envFullSize = size / sizeFactor
                , envCursor = cursor
                }
        let dbToIO =
                case settings ^. Settings.sInfoMode of
                Settings.Evaluation ->
                    EvalManager.runTransactionAndMaybeRestartEvaluators evaluators
                _ -> DbLayout.runDbTransaction db
        widget <- mkWidgetWithFallback dbToIO env
        return . Widget.scale sizeFactor $ Widget.weakerEvents eventMap widget

runEditor :: Maybe FilePath -> Opts.WindowMode -> Db -> IO ()
runEditor mFontPath windowMode db =
    do
        -- GLFW changes the directory from start directory, at least on macs.
        startDir <- Directory.getCurrentDirectory

        -- Load config as early as possible, before we open any windows/etc
        configSampler <- ConfigSampler.new startDir

        GLFWUtils.withGLFW $ do
            win <- createWindow windowMode
            Font.with startDir mFontPath $ \font -> do
                -- Fonts must be loaded after the GL context is created..
                wrapFlyNav <- FlyNav.makeIO Style.flyNav WidgetIds.flyNav
                invalidateCacheRef <- newIORef (return ())
                let invalidateCache = join (readIORef invalidateCacheRef)
                evaluators <- EvalManager.new invalidateCache db
                zoom <- Zoom.make =<< GLFWUtils.getDisplayScale win
                settingsRef <- Settings Settings.defaultInfoMode & newIORef
                (invalidateCacheAction, makeRootWidgetCached) <-
                    makeRootWidget font db zoom settingsRef evaluators
                    & memoizeMakeWidget
                refreshScheduler <- newRefreshScheduler
                writeIORef invalidateCacheRef $
                    do
                        invalidateCacheAction
                        scheduleRefresh refreshScheduler
                EvalManager.start evaluators

                addHelp <- EventMapDoc.makeToggledHelpAdder EventMapDoc.HelpNotShown
                mainLoop win refreshScheduler configSampler $
                    \config size ->
                    makeRootWidgetCached (config, size)
                    >>= wrapFlyNav
                    >>= addHelp (Style.help font (Config.help config)) size

newtype RefreshScheduler = RefreshScheduler (IORef Bool)
newRefreshScheduler :: IO RefreshScheduler
newRefreshScheduler = newIORef False <&> RefreshScheduler
shouldRefresh :: RefreshScheduler -> IO Bool
shouldRefresh (RefreshScheduler ref) = atomicModifyIORef ref $ \r -> (False, r)
scheduleRefresh :: RefreshScheduler -> IO ()
scheduleRefresh (RefreshScheduler ref) = writeIORef ref True

mainLoop ::
    GLFW.Window -> RefreshScheduler -> Sampler ->
    (Config -> Widget.Size -> IO (Widget IO)) -> IO ()
mainLoop win refreshScheduler configSampler iteration =
    do
        lastVersionNumRef <- newIORef =<< getCurrentTime
        let getAnimHalfLife =
                ConfigSampler.getConfig configSampler <&> Style.anim . snd
            makeWidget size =
                do
                    (_, config) <- ConfigSampler.getConfig configSampler
                    iteration config size
            tickHandler =
                do
                    (curVersionNum, _) <- ConfigSampler.getConfig configSampler
                    configChanged <- atomicModifyIORef lastVersionNumRef $ \lastVersionNum ->
                        (curVersionNum, lastVersionNum /= curVersionNum)
                    if configChanged
                        then return True
                        else shouldRefresh refreshScheduler
        mainLoopWidget win tickHandler makeWidget getAnimHalfLife

memoizeMakeWidget :: Eq a => (a -> IO (Widget IO)) -> IO (IO (), a -> IO (Widget IO))
memoizeMakeWidget mkWidget =
    do
        widgetCacheRef <- newIORef =<< memoIO mkWidget
        let invalidateCache = writeIORef widgetCacheRef =<< memoIO mkWidget
        return
            ( invalidateCache
            , \x ->
                readIORef widgetCacheRef
                >>= ($ x)
                <&> Widget.events %~ (<* invalidateCache)
            )

rootCursor :: Widget.Id
rootCursor = WidgetIds.fromGuid $ IRef.guid $ DbLayout.panes DbLayout.codeIRefs

mkWidgetWithFallback ::
    (forall a. Transaction DbLayout.DbM a -> IO a) ->
    GUIMain.Env -> IO (Widget IO)
mkWidgetWithFallback dbToIO env =
    do
        (isValid, widget) <-
            dbToIO $
            do
                candidateWidget <- makeMainGui dbToIO env
                (isValid, widget) <-
                    if candidateWidget ^. Widget.isFocused
                    then return (True, candidateWidget)
                    else do
                        finalWidget <- makeMainGui dbToIO env { GUIMain.envCursor = rootCursor }
                        Transaction.setP (DbLayout.cursor DbLayout.revisionProps) rootCursor
                        return (False, finalWidget)
                unless (widget ^. Widget.isFocused) $
                    fail "Root cursor did not match"
                return (isValid, widget)
        unless isValid $ putStrLn $ "Invalid cursor: " ++ show (GUIMain.envCursor env)
        widget
            & Widget.backgroundColor
              (Config.layerMax (Config.layers config))
              ["background"] (bgColor isValid config)
            & return
    where
        config = GUIMain.envConfig env
        bgColor False = Config.invalidCursorBGColor
        bgColor True = Config.backgroundColor

makeMainGui ::
    (forall a. Transaction DbLayout.DbM a -> f a) ->
    GUIMain.Env -> Transaction DbLayout.DbM (Widget f)
makeMainGui runTransaction env =
    GUIMain.make env rootCursor
    <&> Widget.events %~ runTransaction . (attachCursor =<<)
    where
        attachCursor eventResult =
            do
                maybe (return ()) (Transaction.setP (DbLayout.cursor DbLayout.revisionProps)) .
                    Monoid.getLast $ eventResult ^. Widget.eCursor
                return eventResult

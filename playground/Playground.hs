module Playground
  ( myMain,
  )
where

import Control.Monad
import Data.Coerce
import Data.IORef
import Data.List (maximumBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import GHC
import GHC.Core.Type (tyConAppTyCon_maybe)
import GHC.Driver.Backend (interpreterBackend)
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Monad
import GHC.Plugins
import GHC.Runtime.Interpreter
import GHC.Types.Name (getSrcSpan)
import GHC.Types.SrcLoc (SrcSpan (..), srcSpanStartCol, srcSpanStartLine)
import GHC.Utils.Exception
import GHC.Wasm.Prim
import System.IO (hPutStrLn, stderr)

newtype JSFunction t = JSFunction JSVal

type ExportedMainFunction = JSString -> JSString -> IO ()

-- | Sortable start position for a binding's source span. Bindings without a
-- real span (which shouldn't happen for top-level definitions) sort first so
-- they are never picked as the "last defined" one.
spanKey :: SrcSpan -> (Int, Int)
spanKey (RealSrcSpan s _) = (srcSpanStartLine s, srcSpanStartCol s)
spanKey _ = (minBound, minBound)

-- | Main entry point. Takes the GHC libdir path and a space-separated
-- list of extra package DB paths, returns a JS async function that
-- takes GHC args and Haskell source code, then compiles the module,
-- finds the last top-level binding of type @Waterfall.Solids.Solid@, and
-- writes it out as a GLB for the viewer to display.
myMain :: JSString -> JSString -> IO (JSFunction ExportedMainFunction)
myMain js_libdir js_pkg_dbs =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    libdir <- evaluate $ fromJSString js_libdir
    freeJSVal $ coerce js_libdir
    pkg_db_paths <- evaluate $ words $ fromJSString js_pkg_dbs
    freeJSVal $ coerce js_pkg_dbs
    session <- Session <$> newIORef undefined
    dflags0 <- flip reflectGhc session $ do
      initGhcMonad (Just libdir)
      dflags0 <- getSessionDynFlags
      setSessionDynFlags $
        dflags0
          { ghcMode = CompManager,
            backend = interpreterBackend,
            ghcLink = LinkInMemory,
            verbosity = 1,
            packageDBFlags = packageDBFlags dflags0 ++
              map (PackageDB . PkgDbPath) pkg_db_paths
          }
      getSessionDynFlags
    toMainFunc $ \js_args js_src ->
      defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
        args <- evaluate $ words $ fromJSString js_args
        freeJSVal $ coerce js_args
        writeFile f $ fromJSString js_src
        freeJSVal $ coerce js_src
        flip reflectGhc session $ withCleanupSession $ do
          setSessionDynFlags dflags0
          logger0 <- getLogger
          (dflags1, _, dynamicFlagWarnings) <-
            parseDynamicFlags logger0 dflags0 $ map noLoc args
          setSessionDynFlags dflags1
          logger1 <- getLogger
          liftIO
            $ printOrThrowDiagnostics
              logger1
              (initPrintConfig dflags1)
              (initDiagOpts dflags1)
            $ GhcDriverMessage
              <$> dynamicFlagWarnings
          setTargets =<< (: []) <$> guessTarget f Nothing Nothing
          r <- load LoadAllTargets
          when (failed r) $ fail "load returned Failed"
          mainMod <- findModule (mkModuleName "Main") Nothing
          setContext
            [ IIDecl $ simpleImportDecl $ mkModuleName "Main",
              IIDecl $ simpleImportDecl $ mkModuleName "Waterfall.Solids",
              IIDecl $ simpleImportDecl $ mkModuleName "Waterfall.IO"
            ]
          -- Identify the Solid TyCon from a value known to be a Solid, so we
          -- avoid depending on how (or whether) the user imported the type.
          solidTy <- GHC.exprType TM_Inst "Waterfall.Solids.unitSphere"
          let solidTyCon = tyConAppTyCon_maybe solidTy
          mbModInfo <- getModuleInfo mainMod
          modInfo <- maybe (fail "could not get module info for Main") pure mbModInfo
          things <- catMaybes <$> mapM lookupName (modInfoExports modInfo)
          let solidIds =
                [ i
                  | AnId i <- things,
                    nameModule (getName i) == mainMod,
                    tyConAppTyCon_maybe (idType i) == solidTyCon
                ]
          case solidIds of
            [] ->
              liftIO $
                hPutStrLn stderr
                  "No top-level binding of type Waterfall.Solids.Solid was found."
            _ -> do
              let chosen = maximumBy (comparing (spanKey . getSrcSpan)) solidIds
                  nm = getOccString (getName chosen)
              liftIO $ putStrLn $ "Rendering: " ++ nm
              fhv <-
                compileExprRemote $
                  "Waterfall.IO.writeGLB 0.01 \"/out.glb\" (Main." ++ nm ++ ")"
              hsc_env <- getSession
              liftIO $ evalIO (hscInterp hsc_env) fhv
  where
    f = "/tmp/Main.hs"

foreign import javascript "wrapper"
  toMainFunc ::
    ExportedMainFunction ->
    IO (JSFunction ExportedMainFunction)

foreign export javascript "myMain"
  myMain ::
    JSString ->
    JSString ->
    IO
      (JSFunction ExportedMainFunction)

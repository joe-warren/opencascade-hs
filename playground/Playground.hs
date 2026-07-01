module Playground
  ( myMain,
  )
where

import Control.Monad
import Data.Coerce
import Data.IORef
import Data.List (intercalate, sortBy)
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

-- | Compile the user's source and return the names of its top-level
-- Solid-valued bindings (JSON array, source order).
type RunFunction = JSString -> JSString -> IO JSString

-- | Render a named top-level Solid binding to /out.glb.
type RenderFunction = JSString -> IO ()

-- | Sortable start position for a binding's source span. Bindings without a
-- real span (which shouldn't happen for top-level definitions) sort first.
spanKey :: SrcSpan -> (Int, Int)
spanKey (RealSrcSpan s _) = (srcSpanStartLine s, srcSpanStartCol s)
spanKey _ = (minBound, minBound)

-- | Encode a list of identifier names as a JSON array. Names are Haskell
-- identifiers, so no escaping beyond the surrounding quotes is required.
namesToJson :: [String] -> String
namesToJson ns = "[" ++ intercalate "," (map quote ns) ++ "]"
  where
    quote s = "\"" ++ s ++ "\""

-- | Main entry point. Takes the GHC libdir path and a space-separated list of
-- extra package DB paths, and returns a JS array @[run, render]@:
--
--   * @run(args, src)@ compiles the module and returns the JSON list of
--     top-level bindings whose type is @Waterfall.Solids.Solid@.
--   * @render(name)@ writes the chosen binding out as /out.glb for the viewer.
myMain :: JSString -> JSString -> IO JSVal
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

    runFn <- toRunFunc $ \js_args js_src ->
      defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
        args <- evaluate $ words $ fromJSString js_args
        freeJSVal $ coerce js_args
        writeFile f $ fromJSString js_src
        freeJSVal $ coerce js_src
        names <- flip reflectGhc session $ withCleanupSession $ do
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
              ordered = sortBy (comparing (spanKey . getSrcSpan)) solidIds
          pure $ map (getOccString . getName) ordered
        pure $ toJSString $ namesToJson names

    renderFn <- toRenderFunc $ \js_name ->
      defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
        name <- evaluate $ fromJSString js_name
        freeJSVal $ coerce js_name
        flip reflectGhc session $ do
          liftIO $ putStrLn $ "Rendering: " ++ name
          fhv <-
            compileExprRemote $
              "Waterfall.IO.writeGLB 0.01 \"/out.glb\" (Main." ++ name ++ ")"
          hsc_env <- getSession
          liftIO $ evalIO (hscInterp hsc_env) fhv

    mkPair runFn renderFn
  where
    f = "/tmp/Main.hs"

foreign import javascript "wrapper"
  toRunFunc :: RunFunction -> IO JSVal

foreign import javascript "wrapper"
  toRenderFunc :: RenderFunction -> IO JSVal

foreign import javascript unsafe "[$1, $2]"
  mkPair :: JSVal -> JSVal -> IO JSVal

foreign export javascript "myMain"
  myMain :: JSString -> JSString -> IO JSVal

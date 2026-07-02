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
import GHC.Core.TyCon (tyConName)
import GHC.Core.Type (tyConAppTyCon_maybe)
import GHC.Driver.Backend (interpreterBackend)
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Monad
import GHC.Plugins
import GHC.Runtime.Interpreter
import GHC.Types.Name (getSrcSpan, nameModule_maybe)
import GHC.Unit.Module.Graph (mgModSummaries)
import GHC.Unit.Module.ModSummary (ms_mod)
import GHC.Types.SrcLoc (SrcSpan (..), srcSpanStartCol, srcSpanStartLine)
import GHC.Utils.Exception
import GHC.Wasm.Prim

-- | Compile the user's source and return the names of its top-level
-- Solid-valued bindings (JSON array, source order).
type RunFunction = JSString -> JSString -> IO JSString

-- | Render a named top-level Solid binding to /out.glb, at a given mesh
-- deflection (resolution; smaller = finer). Args: name, deflection.
type RenderFunction = JSString -> JSString -> IO ()

-- | Write a named top-level Solid binding to a path; the format is chosen from
-- the file extension by @Waterfall.IO.writeSolid@ (.stl/.step/.gltf/.glb/.obj).
-- Args: name, path, deflection (resolution).
type ExportFunction = JSString -> JSString -> JSString -> IO ()

-- | Sortable start position for a binding's source span. Bindings without a
-- real span (which shouldn't happen for top-level definitions) sort first.
spanKey :: SrcSpan -> (Int, Int)
spanKey (RealSrcSpan s _) = (srcSpanStartLine s, srcSpanStartCol s)
spanKey _ = (minBound, minBound)

-- | True when the type's head is Waterfall's Solid newtype. Checked by the
-- TyCon's original name so we don't need Solid in the interactive scope.
isSolidType :: Type -> Bool
isSolidType ty = case tyConAppTyCon_maybe ty of
  Just tc ->
    let n = tyConName tc
     in occNameString (nameOccName n) == "Solid"
          && maybe
            False
            ((== "Waterfall.Internal.Solid") . moduleNameString . moduleName)
            (nameModule_maybe n)
  Nothing -> False

-- | Encode a list of identifier names as a JSON array.
namesToJson :: [String] -> String
namesToJson ns = "[" ++ intercalate "," (map quote ns) ++ "]"
  where
    quote s = "\"" ++ s ++ "\""

-- | Main entry point. Takes the GHC libdir path and a space-separated list of
-- extra package DB paths, and returns a JS array @[run, render, export]@:
--
--   * @run(args, src)@ compiles the module and returns the JSON list of
--     top-level bindings whose type is @Waterfall.Solids.Solid@.
--   * @render(name, res)@ writes the binding to /out.glb at deflection @res@.
--   * @export(name, path, res)@ writes to @path@ (format by extension) at @res@.
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
          -- The user's module may or may not carry a `module ... where` header,
          -- so use whatever name it actually compiled to rather than assuming
          -- Main. Package deps aren't in the graph, so the single home module
          -- is the user's.
          graph <- getModuleGraph
          userMod <- case mgModSummaries graph of
            (ms : _) -> pure (ms_mod ms)
            [] -> fail "no module was loaded"
          -- Bring the module's *full* top-level scope into the interactive
          -- context (like GHCi's `*Module`), so we see every top-level binding
          -- regardless of the export list. This covers a headerless file, which
          -- GHC treats as `module Main (main) where` -- an export list of just
          -- `main` -- as well as any explicit export list that omits solids.
          setContext
            [ IIModule userMod,
              IIDecl $ simpleImportDecl $ mkModuleName "Waterfall.IO"
            ]
          inScope <- getNamesInScope
          things <-
            catMaybes
              <$> mapM lookupName
                (filter ((== Just userMod) . nameModule_maybe) inScope)
          let solidIds = [i | AnId i <- things, isSolidType (idType i)]
              ordered = sortBy (comparing (spanKey . getSrcSpan)) solidIds
          pure $ map (getOccString . getName) ordered
        pure $ toJSString $ namesToJson names

    renderFn <- toRenderFunc $ \js_name js_res ->
      defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
        name <- evaluate $ fromJSString js_name
        freeJSVal $ coerce js_name
        res <- evaluate $ fromJSString js_res
        freeJSVal $ coerce js_res
        flip reflectGhc session $ do
          liftIO $ putStrLn $ "Rendering: " ++ name
          -- `name` is in scope unqualified via the IIModule context set in `run`
          -- (so this also reaches bindings the module doesn't export). `res` is a
          -- numeric literal (mesh deflection) supplied by the resolution setting.
          fhv <-
            compileExprRemote $
              "Waterfall.IO.writeGLB " ++ res ++ " \"/out.glb\" (" ++ name ++ ")"
          hsc_env <- getSession
          liftIO $ evalIO (hscInterp hsc_env) fhv

    exportFn <- toExportFunc $ \js_name js_path js_res ->
      defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
        name <- evaluate $ fromJSString js_name
        freeJSVal $ coerce js_name
        path <- evaluate $ fromJSString js_path
        freeJSVal $ coerce js_path
        res <- evaluate $ fromJSString js_res
        freeJSVal $ coerce js_res
        flip reflectGhc session $ do
          -- writeSolid picks the writer from the file extension. `name` is in
          -- scope unqualified via the IIModule context set in `run`.
          fhv <-
            compileExprRemote $
              "Waterfall.IO.writeSolid " ++ res ++ " \"" ++ path ++ "\" (" ++ name ++ ")"
          hsc_env <- getSession
          liftIO $ evalIO (hscInterp hsc_env) fhv

    mkTriple runFn renderFn exportFn
  where
    f = "/tmp/Main.hs"

foreign import javascript "wrapper"
  toRunFunc :: RunFunction -> IO JSVal

foreign import javascript "wrapper"
  toRenderFunc :: RenderFunction -> IO JSVal

foreign import javascript "wrapper"
  toExportFunc :: ExportFunction -> IO JSVal

foreign import javascript unsafe "[$1, $2, $3]"
  mkTriple :: JSVal -> JSVal -> JSVal -> IO JSVal

foreign export javascript "myMain"
  myMain :: JSString -> JSString -> IO JSVal

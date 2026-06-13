module Playground
  ( myMain,
  )
where

import Control.Monad
import Data.Coerce
import Data.IORef
import GHC
import GHC.Driver.Backend (interpreterBackend)
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Monad
import GHC.Plugins
import GHC.Runtime.Interpreter
import GHC.Utils.Exception
import GHC.Wasm.Prim

newtype JSFunction t = JSFunction JSVal

type ExportedMainFunction = JSString -> JSString -> IO ()

-- | Main entry point. Takes the GHC libdir path and a space-separated
-- list of extra package DB paths, returns a JS async function that
-- takes GHC args and Haskell source code, compiles and runs Main.main.
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
          setContext [IIDecl $ simpleImportDecl $ mkModuleName "Main"]
          fhv <- compileExprRemote "Main.main"
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

{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module OpenCascade.Standard.Failure
( Failure
, getMessageString
, catch
) where

import OpenCascade.Standard.Types (Failure)
import Foreign.Ptr
import Foreign.C.String (CString, peekCString)
import Control.Monad ((<=<))

foreign import capi unsafe "hs_Standard_Failure.h hs_Standard_Failure_getMessageString" rawGetMessageString :: Ptr Failure -> IO (CString)

getMessageString :: Ptr Failure -> IO String
getMessageString = peekCString <=< rawGetMessageString

foreign import ccall "wrapper" mkCallback :: IO () -> IO (FunPtr (IO ()))

foreign import ccall "wrapper" mkHandlerCallback :: (Ptr Failure -> IO ()) -> IO (FunPtr (Ptr Failure -> IO ()))

foreign import capi safe "hs_Standard_Failure.h hs_Standard_Failure_catch" rawCatch :: FunPtr (IO ()) -> FunPtr (Ptr Failure -> IO ()) -> IO ()

catch :: IO () -> (Ptr Failure -> IO ()) -> IO ()
catch f handler = do
    addrF <- mkCallback f
    addrHandler <- mkHandlerCallback handler
    rawCatch addrF addrHandler
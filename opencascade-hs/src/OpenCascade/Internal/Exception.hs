{-# LANGUAGE CApiFFI #-}
module OpenCascade.Internal.Exception 
( OpenCascadeException (..)
, wrapException
, testThrow
) where

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal (alloca)
import Control.Exception (Exception, throw)
import Data.Data (Typeable)
import Foreign.Storable (peek)
import qualified OpenCascade.Standard.Failure as Standard.Failure
import qualified OpenCascade.Std.Types as Std
import qualified OpenCascade.Std.Exception as Std.Exception
import OpenCascade.Standard.Internal.Destructors (deleteFailure)
import OpenCascade.Std.Internal.Destructors (deleteException)

-- This needs to stay consistent with the ordering in hs_Exception.h
data ExceptionType = NoException | StandardFailureException | StdException | OtherException 
    deriving (Show, Eq, Ord, Enum, Bounded)

data OpenCascadeException 
    = OpenCascadeStandardFailure String String
    | OpenCascadeStdException String
    | OpenCascadeOtherException
    deriving (Eq, Ord, Typeable, Show)

instance Exception OpenCascadeException

safePeekCString :: Ptr CChar -> IO String
safePeekCString ptr = 
    if ptr == nullPtr
        then pure ""
        else peekCString ptr

handleError :: Ptr CInt -> Ptr (Ptr ()) -> IO ()
handleError flagPtr exPtr = do
    flag <- peek flagPtr
    case toEnum . fromIntegral $ flag of 
        NoException -> pure ()
        StandardFailureException -> do
            stdFailure <- castPtr <$> peek exPtr
            msgString <- safePeekCString =<< Standard.Failure.getMessageString stdFailure
            stackString <- safePeekCString =<< Standard.Failure.getStackString stdFailure
            deleteFailure stdFailure
            throw (OpenCascadeStandardFailure msgString stackString)
        StdException -> do
            stdException <- castPtr <$> peek exPtr
            msgString <- safePeekCString =<< Std.Exception.what stdException
            deleteException stdException
            throw (OpenCascadeStdException msgString)
        OtherException -> do
            throw OpenCascadeOtherException

wrapException :: (Ptr CInt -> Ptr (Ptr ()) -> IO a) -> IO a
wrapException f =
    alloca $ \flagPtr -> 
        alloca $ \exPtr -> do
            f flagPtr exPtr <* handleError flagPtr exPtr

foreign import capi unsafe "hs_Exception.h hs_throw_std_exception" rawThrow :: Ptr Std.Exception -> Ptr CInt -> Ptr (Ptr ())-> IO ()

-- | This is exposed in order to write tests for the Std.Exception catching path
testThrow :: Ptr Std.Exception -> IO ()
testThrow ex = wrapException (rawThrow ex)
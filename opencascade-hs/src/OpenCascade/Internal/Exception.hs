module OpenCascade.Internal.Exception 
( wrapException
) where

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal (alloca)
-- This needs to stay consistent with the ordering in hs_Exception.h
data ExceptionType = NoException | StandardFailureException | StdException | OtherException 
    deriving (Show, Eq, Ord, Enum, Bounded)

wrapException :: (Ptr CInt -> Ptr (Ptr ()) -> IO a) -> IO a
wrapException f =
    alloca $ \flagPtr -> 
        alloca $ \exPtr -> do
            f flagPtr exPtr

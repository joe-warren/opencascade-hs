{-# LANGUAGE CApiFFI #-}
module OpenCascade.Std.Internal.Destructors 
( deleteException
) where

import OpenCascade.Std.Types (Exception)
import Foreign.Ptr (Ptr)

foreign import capi unsafe "hs_Exception.h hs_delete_std_exception" deleteException :: Ptr Exception -> IO ()
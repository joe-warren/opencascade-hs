{-# LANGUAGE CApiFFI #-}
module OpenCascade.Std.Exception
( Exception
, what
) where

import OpenCascade.Std.Types (Exception)
import Foreign.C (CString)
import Foreign.Ptr (Ptr)

foreign import capi unsafe "hs_Exception.h hs_std_exception_what" what :: Ptr Exception -> IO CString
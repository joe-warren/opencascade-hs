{-# LANGUAGE CApiFFI #-}
module OpenCascade.Std.RuntimeError 
( RuntimeError
, new
) where 

import OpenCascade.Std.Internal.Destructors (deleteException)
import OpenCascade.Std.Types (RuntimeError)
import Data.Acquire (Acquire, mkAcquire)
import Foreign.Ptr (Ptr)
import Foreign.C.String (CString, withCString)
import OpenCascade.Inheritance (upcast)

foreign import capi unsafe "hs_Exception.h hs_new_runtime_error" rawNew :: CString -> IO (Ptr RuntimeError)

new :: String -> Acquire (Ptr RuntimeError)
new s = mkAcquire (withCString s rawNew) (deleteException . upcast)

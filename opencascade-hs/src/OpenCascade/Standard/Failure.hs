{-# LANGUAGE CApiFFI #-}
module OpenCascade.Standard.Failure
( Failure
, getStackString 
) where

import OpenCascade.Standard.Types (Failure)
import Foreign.C (CString)
import Foreign.Ptr (Ptr)

foreign import capi unsafe "hs_Standard_Failure.h hs_Standard_Failure_GetStackString" getStackString :: Ptr Failure -> IO CString

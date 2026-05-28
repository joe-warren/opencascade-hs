{-# LANGUAGE CApiFFI #-}
module OpenCascade.Standard.Failure
( Failure
, getMessageString
, getStackString 
) where

import OpenCascade.Standard.Types (Failure)
import Foreign.C (CString)
import Foreign.Ptr (Ptr)

foreign import capi unsafe "hs_Standard_Failure.h hs_Standard_Failure_GetMessageString" getMessageString :: Ptr Failure -> IO CString

foreign import capi unsafe "hs_Standard_Failure.h hs_Standard_Failure_GetStackString" getStackString :: Ptr Failure -> IO CString

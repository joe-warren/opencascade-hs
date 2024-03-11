{-# LANGUAGE CApiFFI #-}
module OpenCascade.TColStd.Internal.Destructors
( deleteIndexedDataMapOfStringString
) where

import OpenCascade.TColStd.Types
import Foreign.Ptr 

foreign import capi unsafe "hs_TColStd_IndexedDataMapOfStringString.h hs_delete_TColStd_IndexedDataMapOfStringString" deleteIndexedDataMapOfStringString :: Ptr (IndexedDataMapOfStringString) -> IO ()
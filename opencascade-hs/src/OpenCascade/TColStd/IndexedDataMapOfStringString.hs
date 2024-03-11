{-# LANGUAGE CApiFFI #-}
module OpenCascade.TColStd.IndexedDataMapOfStringString
( IndexedDataMapOfStringString
, new
) where

import OpenCascade.TColStd.Types (IndexedDataMapOfStringString)
import OpenCascade.TColStd.Internal.Destructors (deleteIndexedDataMapOfStringString)
import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_TColStd_IndexedDataMapOfStringString.h hs_new_TColStd_IndexedDataMapOfStringString" rawNew :: IO (Ptr IndexedDataMapOfStringString)

new :: Acquire (Ptr IndexedDataMapOfStringString)
new = mkAcquire rawNew deleteIndexedDataMapOfStringString
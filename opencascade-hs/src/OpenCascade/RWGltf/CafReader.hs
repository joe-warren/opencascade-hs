{-# LANGUAGE CApiFFI #-}
module OpenCascade.RWGltf.CafReader 
( CafReader
, new
) where

import OpenCascade.RWGltf.Types (CafReader)
import OpenCascade.RWGltf.Internal.Destructors (deleteCafReader)
import Data.Acquire (Acquire, mkAcquire)
import Foreign.Ptr (Ptr)

foreign import capi unsafe "hs_RWGltf_CafReader.h hs_new_RWGltf_CafReader" rawNew :: IO (Ptr CafReader)

new :: Acquire (Ptr CafReader)
new = mkAcquire rawNew deleteCafReader
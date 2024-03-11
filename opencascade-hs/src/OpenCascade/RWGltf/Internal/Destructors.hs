{-# LANGUAGE CApiFFI #-}
module OpenCascade.RWGltf.Internal.Destructors
( deleteCafWriter
) where

import OpenCascade.RWGltf.Types
import Foreign.Ptr 

foreign import capi unsafe "hs_RWGltf_CafWriter.h hs_delete_RWGltf_CafWriter" deleteCafWriter :: Ptr CafWriter -> IO ()
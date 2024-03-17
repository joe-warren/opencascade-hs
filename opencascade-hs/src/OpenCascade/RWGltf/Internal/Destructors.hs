{-# LANGUAGE CApiFFI #-}
module OpenCascade.RWGltf.Internal.Destructors
( deleteCafWriter
, deleteCafReader
) where

import OpenCascade.RWGltf.Types
import Foreign.Ptr 

foreign import capi unsafe "hs_RWGltf_CafWriter.h hs_delete_RWGltf_CafWriter" deleteCafWriter :: Ptr CafWriter -> IO ()
foreign import capi unsafe "hs_RWGltf_CafReader.h hs_delete_RWGltf_CafReader" deleteCafReader :: Ptr CafReader -> IO ()
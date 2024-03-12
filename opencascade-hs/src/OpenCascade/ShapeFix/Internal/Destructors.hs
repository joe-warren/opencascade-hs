{-# LANGUAGE CApiFFI #-}
module OpenCascade.ShapeFix.Internal.Destructors (deleteSolid) where

import OpenCascade.ShapeFix.Types (Solid)
import Foreign.Ptr (Ptr)

foreign import capi unsafe "hs_ShapeFix_Solid.h hs_delete_ShapeFix_Solid" deleteSolid :: Ptr Solid -> IO ()
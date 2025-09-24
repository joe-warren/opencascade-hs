{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepTools.Internal.Destructors
( deleteWireExplorer
) where

import OpenCascade.BRepTools.Types
import Foreign.Ptr

foreign import capi unsafe "hs_BRepTools_WireExplorer.h hs_delete_BRepTools_WireExplorer" deleteWireExplorer :: Ptr WireExplorer -> IO ()
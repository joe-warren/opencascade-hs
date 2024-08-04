{-# LANGUAGE CApiFFI #-}
module OpenCascade.ShapeExtend.Internal.Destructors
( deleteWireData
) where

import OpenCascade.ShapeExtend.Types (WireData)

import Foreign.Ptr

foreign import capi unsafe "hs_ShapeExtend_WireData.h hs_delete_ShapeExtend_WireData" deleteWireData :: Ptr WireData -> IO ()
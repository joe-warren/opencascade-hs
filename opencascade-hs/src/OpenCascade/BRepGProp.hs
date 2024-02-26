{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepGProp (volumeProperties) where

import OpenCascade.TopoDS.Types (Shape)
import OpenCascade.GProp.Types (GProps)
import Foreign.Ptr (Ptr)
import Foreign.C (CBool (..))
import OpenCascade.Internal.Bool (boolToCBool)

foreign import capi unsafe "hs_BRepGProp.h hs_BRepGProp_VolumeProperties" rawVolumeProperties :: Ptr Shape -> Ptr GProps -> CBool -> CBool -> CBool -> IO ()

volumeProperties :: Ptr Shape -> Ptr GProps -> Bool -> Bool -> Bool -> IO ()
volumeProperties shape props onlyClosed skipShared useTriangulation =
    rawVolumeProperties shape props (boolToCBool onlyClosed) (boolToCBool skipShared) (boolToCBool useTriangulation)
 
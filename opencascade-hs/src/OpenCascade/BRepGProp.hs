{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepGProp 
( volumeProperties
, surfaceProperties
, linearProperties
) where

import OpenCascade.TopoDS.Types (Shape)
import OpenCascade.GProp.Types (GProps)
import Foreign.Ptr (Ptr)
import Foreign.C (CBool (..))
import OpenCascade.Internal.Bool (boolToCBool)

foreign import capi unsafe "hs_BRepGProp.h hs_BRepGProp_VolumeProperties" rawVolumeProperties :: Ptr Shape -> Ptr GProps -> CBool -> CBool -> CBool -> IO ()

volumeProperties :: Ptr Shape -> Ptr GProps -> Bool -> Bool -> Bool -> IO ()
volumeProperties shape props onlyClosed skipShared useTriangulation =
    rawVolumeProperties shape props (boolToCBool onlyClosed) (boolToCBool skipShared) (boolToCBool useTriangulation)

foreign import capi unsafe "hs_BRepGProp.h hs_BRepGProp_SurfaceProperties" rawSurfaceProperties :: Ptr Shape -> Ptr GProps -> CBool -> CBool -> IO ()

surfaceProperties :: Ptr Shape -> Ptr GProps -> Bool -> Bool -> IO ()
surfaceProperties shape props skipShared useTriangulation =
    rawSurfaceProperties shape props (boolToCBool skipShared) (boolToCBool useTriangulation)

    
foreign import capi unsafe "hs_BRepGProp.h hs_BRepGProp_LinearProperties" rawLinearProperties :: Ptr Shape -> Ptr GProps -> CBool -> CBool -> IO ()

linearProperties :: Ptr Shape -> Ptr GProps -> Bool -> Bool -> IO ()
linearProperties shape props skipShared useTriangulation =
    rawLinearProperties shape props (boolToCBool skipShared) (boolToCBool useTriangulation)
 
 
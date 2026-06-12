{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepGProp 
( volumeProperties
, surfaceProperties
, linearProperties
) where

import OpenCascade.TopoDS.Types (Shape)
import OpenCascade.GProp.Types (GProps)
import Foreign.Ptr (Ptr)
import Foreign.C (CBool (..), CInt)
import OpenCascade.Internal.Bool (boolToCBool)
import OpenCascade.Internal.Exception (wrapException)

foreign import capi unsafe "hs_BRepGProp.h hs_BRepGProp_VolumeProperties" rawVolumeProperties
    :: Ptr Shape
    -> Ptr GProps
    -> CBool
    -> CBool
    -> CBool
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

volumeProperties :: Ptr Shape -> Ptr GProps -> Bool -> Bool -> Bool -> IO ()
volumeProperties shape props onlyClosed skipShared useTriangulation =
    wrapException $ rawVolumeProperties shape props (boolToCBool onlyClosed) (boolToCBool skipShared) (boolToCBool useTriangulation)

foreign import capi unsafe "hs_BRepGProp.h hs_BRepGProp_SurfaceProperties" rawSurfaceProperties
    :: Ptr Shape
    -> Ptr GProps
    -> CBool
    -> CBool
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

surfaceProperties :: Ptr Shape -> Ptr GProps -> Bool -> Bool -> IO ()
surfaceProperties shape props skipShared useTriangulation =
    wrapException $ rawSurfaceProperties shape props (boolToCBool skipShared) (boolToCBool useTriangulation)


foreign import capi unsafe "hs_BRepGProp.h hs_BRepGProp_LinearProperties" rawLinearProperties
    :: Ptr Shape
    -> Ptr GProps
    -> CBool
    -> CBool
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

linearProperties :: Ptr Shape -> Ptr GProps -> Bool -> Bool -> IO ()
linearProperties shape props skipShared useTriangulation =
    wrapException $ rawLinearProperties shape props (boolToCBool skipShared) (boolToCBool useTriangulation)
 
 
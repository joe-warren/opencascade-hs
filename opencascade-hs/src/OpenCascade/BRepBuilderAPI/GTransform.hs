{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepBuilderAPI.GTransform 
( gtransform
) where

import qualified OpenCascade.GP as GP
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.Internal.Bool
import qualified OpenCascade.TopoDS.Internal.Destructors as TopoDS.Destructors
import OpenCascade.Internal.Exception (wrapException)
import Foreign.C
import Foreign.Ptr
import Data.Acquire

foreign import capi unsafe "hs_BRepBuilderAPI_GTransform.h hs_BRepBuilderAPI_GTransform_gtransform" rawGTransform
    :: Ptr TopoDS.Shape
    -> Ptr GP.GTrsf
    -> CBool
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Shape)

gtransform :: Ptr TopoDS.Shape -> Ptr GP.GTrsf -> Bool -> Acquire (Ptr TopoDS.Shape)
gtransform shape trsf copy = mkAcquire (wrapException $ rawGTransform shape trsf (boolToCBool copy)) TopoDS.Destructors.deleteShape

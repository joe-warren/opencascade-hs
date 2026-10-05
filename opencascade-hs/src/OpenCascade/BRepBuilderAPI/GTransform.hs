{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepBuilderAPI.GTransform 
( GTransform
, fromShapeAndGTrsf
) where

import OpenCascade.BRepBuilderAPI.Types (GTransform)
import OpenCascade.BRepBuilderAPI.Internal.Destructors (deleteGTransform)
import qualified OpenCascade.GP as GP
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.Internal.Bool
import OpenCascade.Internal.Exception (wrapException)
import Foreign.C
import Foreign.Ptr
import Data.Acquire

foreign import capi unsafe "hs_BRepBuilderAPI_GTransform.h hs_new_BRepBuilderAPI_GTransform_fromShapeAndGTrsf" rawFromShapeAndGTrsf
    :: Ptr TopoDS.Shape
    -> Ptr GP.GTrsf
    -> CBool
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr GTransform)

fromShapeAndGTrsf :: Ptr TopoDS.Shape -> Ptr GP.GTrsf -> Bool -> Acquire (Ptr GTransform)
fromShapeAndGTrsf shape trsf copy = mkAcquire (wrapException $ rawFromShapeAndGTrsf shape trsf (boolToCBool copy)) deleteGTransform

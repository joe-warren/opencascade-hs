{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepBuilderAPI.Transform 
( Transform
, fromShapeAndTrsf
) where

import OpenCascade.BRepBuilderAPI.Types (Transform)
import OpenCascade.BRepBuilderAPI.Internal.Destructors (deleteTransform)
import qualified OpenCascade.GP as GP
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.Internal.Bool
import OpenCascade.Internal.Exception (wrapException)
import Foreign.C
import Foreign.Ptr
import Data.Acquire

foreign import capi unsafe "hs_BRepBuilderAPI_Transform.h hs_new_BRepBuilderAPI_Transform_fromShapeAndTrsf" rawFromShapeAndTrsf
    :: Ptr TopoDS.Shape
    -> Ptr GP.Trsf
    -> CBool
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr Transform)

fromShapeAndTrsf :: Ptr TopoDS.Shape -> Ptr GP.Trsf -> Bool -> Acquire (Ptr Transform)
fromShapeAndTrsf shape trsf copy = mkAcquire (wrapException $ rawFromShapeAndTrsf shape trsf (boolToCBool copy)) deleteTransform

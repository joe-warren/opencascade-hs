{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepBuilderAPI.Transform 
( transform
) where

import qualified OpenCascade.GP as GP
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.Internal.Bool
import qualified OpenCascade.TopoDS.Internal.Destructors as TopoDS.Destructors
import Foreign.C
import Foreign.Ptr
import Data.Acquire 

foreign import capi unsafe "hs_BRepBuilderAPI_Transform.h hs_BRepBuilderAPI_Transform_transform" rawTransform :: Ptr TopoDS.Shape -> Ptr GP.Trsf -> CBool -> IO (Ptr TopoDS.Shape)

transform :: Ptr TopoDS.Shape -> Ptr GP.Trsf -> Bool -> Acquire (Ptr TopoDS.Shape)
transform shape trsf copy = mkAcquire (rawTransform shape trsf (boolToCBool copy)) TopoDS.Destructors.deleteShape

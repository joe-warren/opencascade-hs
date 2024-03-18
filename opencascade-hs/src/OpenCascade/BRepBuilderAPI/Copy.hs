{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepBuilderAPI.Copy
( copy
) where

import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.Internal.Bool
import qualified OpenCascade.TopoDS.Internal.Destructors as TopoDS.Destructors
import Foreign.C
import Foreign.Ptr
import Data.Acquire 

foreign import capi unsafe "hs_BRepBuilderAPI_Copy.h hs_BRepBuilderAPI_Copy_copy" rawCopy :: Ptr TopoDS.Shape -> CBool -> CBool -> IO (Ptr TopoDS.Shape)

copy :: Ptr TopoDS.Shape -> Bool -> Bool -> Acquire (Ptr TopoDS.Shape)
copy shape copyGeom copyMesh = mkAcquire (rawCopy shape (boolToCBool copyMesh) (boolToCBool copyGeom)) TopoDS.Destructors.deleteShape
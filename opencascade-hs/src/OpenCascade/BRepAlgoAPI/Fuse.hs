{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepAlgoAPI.Fuse
( fuse
) where

import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import Foreign.Ptr
import Data.Acquire


foreign import capi unsafe "hs_BRepAlgoAPI_Fuse.h hs_BRepAlgoAPI_Fuse" rawFuse :: Ptr TopoDS.Shape -> Ptr TopoDS.Shape ->  IO (Ptr TopoDS.Shape)

fuse :: Ptr TopoDS.Shape -> Ptr TopoDS.Shape -> Acquire (Ptr TopoDS.Shape)
fuse a b = mkAcquire (rawFuse a b) deleteShape
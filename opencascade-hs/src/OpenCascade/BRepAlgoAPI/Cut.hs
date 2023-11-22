{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepAlgoAPI.Cut
( cut
) where

import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import Foreign.Ptr
import Data.Acquire


foreign import capi unsafe "hs_BRepAlgoAPI_Cut.h hs_BRepAlgoAPI_Cut" rawCut :: Ptr TopoDS.Shape -> Ptr TopoDS.Shape ->  IO (Ptr TopoDS.Shape)

cut :: Ptr TopoDS.Shape -> Ptr TopoDS.Shape -> Acquire (Ptr TopoDS.Shape)
cut a b = mkAcquire (rawCut a b) deleteShape
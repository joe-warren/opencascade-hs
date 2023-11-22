{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepAlgoAPI.Common
( common
) where

import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import Foreign.Ptr
import Data.Acquire


foreign import capi unsafe "hs_BRepAlgoAPI_Common.h hs_BRepAlgoAPI_Common" rawCommon :: Ptr TopoDS.Shape -> Ptr TopoDS.Shape ->  IO (Ptr TopoDS.Shape)

common :: Ptr TopoDS.Shape -> Ptr TopoDS.Shape -> Acquire (Ptr TopoDS.Shape)
common a b = mkAcquire (rawCommon a b) deleteShape
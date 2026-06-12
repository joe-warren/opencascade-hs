{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepAlgoAPI.Cut
( cut
) where

import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.C (CInt)
import Foreign.Ptr
import Data.Acquire


foreign import capi unsafe "hs_BRepAlgoAPI_Cut.h hs_BRepAlgoAPI_Cut" rawCut
    :: Ptr TopoDS.Shape
    -> Ptr TopoDS.Shape
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Shape)

cut :: Ptr TopoDS.Shape -> Ptr TopoDS.Shape -> Acquire (Ptr TopoDS.Shape)
cut a b = mkAcquire (wrapException $ rawCut a b) deleteShape
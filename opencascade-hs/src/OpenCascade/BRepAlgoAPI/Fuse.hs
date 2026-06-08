{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepAlgoAPI.Fuse
( fuse
) where

import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.C (CInt)
import Foreign.Ptr
import Data.Acquire


foreign import capi unsafe "hs_BRepAlgoAPI_Fuse.h hs_BRepAlgoAPI_Fuse" rawFuse
    :: Ptr TopoDS.Shape
    -> Ptr TopoDS.Shape
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Shape)

fuse :: Ptr TopoDS.Shape -> Ptr TopoDS.Shape -> Acquire (Ptr TopoDS.Shape)
fuse a b = mkAcquire (wrapException $ rawFuse a b) deleteShape
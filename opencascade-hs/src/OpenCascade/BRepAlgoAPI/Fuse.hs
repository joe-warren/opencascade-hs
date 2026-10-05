{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepAlgoAPI.Fuse
( Fuse
, fromShapes
) where

import OpenCascade.BRepAlgoAPI.Types (Fuse)
import OpenCascade.BRepAlgoAPI.Internal.Destructors (deleteFuse)
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.Internal.Exception (wrapException)
import Foreign.C (CInt)
import Foreign.Ptr
import Data.Acquire

foreign import capi unsafe "hs_BRepAlgoAPI_Fuse.h hs_new_BRepAlgoAPI_Fuse_fromShapes" rawFromShapes
    :: Ptr TopoDS.Shape
    -> Ptr TopoDS.Shape
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr Fuse)

fromShapes :: Ptr TopoDS.Shape -> Ptr TopoDS.Shape -> Acquire (Ptr Fuse)
fromShapes a b = mkAcquire (wrapException $ rawFromShapes a b) deleteFuse

{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepOffsetAPI.MakePipe 
( MakePipe
, fromWireAndShape
) where

import OpenCascade.BRepOffsetAPI.Types (MakePipe)
import OpenCascade.BRepOffsetAPI.Internal.Destructors (deleteMakePipe)
import qualified OpenCascade.TopoDS as TopoDS
import Foreign.Ptr
import Data.Acquire

foreign import capi unsafe "hs_BRepOffsetAPI_MakePipe.h hs_new_BRepOffsetAPI_MakePipe_fromWireAndShape" rawFromWireAndShape :: Ptr TopoDS.Wire -> Ptr TopoDS.Shape -> IO (Ptr MakePipe)

fromWireAndShape :: Ptr TopoDS.Wire -> Ptr TopoDS.Shape -> Acquire (Ptr MakePipe)
fromWireAndShape wire profile = mkAcquire (rawFromWireAndShape wire profile) deleteMakePipe
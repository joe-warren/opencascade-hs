{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepOffsetAPI.MakePipe 
( MakePipe
, fromWireAndShape
, fromWireShapeTrihedronModeAndForceC1
) where

import OpenCascade.BRepOffsetAPI.Types (MakePipe)
import OpenCascade.BRepOffsetAPI.Internal.Destructors (deleteMakePipe)
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.GeomFill as GeomFill
import Foreign.Ptr
import Foreign.C (CBool (..), CInt (..))
import OpenCascade.Internal.Bool (boolToCBool)
import Data.Acquire

foreign import capi unsafe "hs_BRepOffsetAPI_MakePipe.h hs_new_BRepOffsetAPI_MakePipe_fromWireAndShape" rawFromWireAndShape :: Ptr TopoDS.Wire -> Ptr TopoDS.Shape -> IO (Ptr MakePipe)

fromWireAndShape :: Ptr TopoDS.Wire -> Ptr TopoDS.Shape -> Acquire (Ptr MakePipe)
fromWireAndShape wire profile = mkAcquire (rawFromWireAndShape wire profile) deleteMakePipe

foreign import capi unsafe "hs_BRepOffsetAPI_MakePipe.h hs_new_BRepOffsetAPI_MakePipe_fromWireShapeTrihedronModeAndForceC1" rawFromWireShapeTrihedronModeAndForceC1 :: Ptr TopoDS.Wire -> Ptr TopoDS.Shape -> CInt -> CBool -> IO (Ptr MakePipe)
fromWireShapeTrihedronModeAndForceC1 :: Ptr TopoDS.Wire -> Ptr TopoDS.Shape -> GeomFill.Trihedron -> Bool -> Acquire (Ptr MakePipe)
fromWireShapeTrihedronModeAndForceC1 wire profile trihedronMode forceC1 =
     mkAcquire (rawFromWireShapeTrihedronModeAndForceC1 wire profile ((fromIntegral . fromEnum $ trihedronMode)) (boolToCBool forceC1)) deleteMakePipe
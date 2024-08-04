{-# LANGUAGE CApiFFI #-}
module OpenCascade.ShapeExtend.WireData
( WireData
, fromWireChainedAndManifold
, reverse
, wire
, wireAPIMake
) where

import Prelude hiding (reverse)
import OpenCascade.ShapeExtend.Types (WireData)
import OpenCascade.ShapeExtend.Internal.Destructors (deleteWireData)
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.Internal.Bool (boolToCBool)
import OpenCascade.Inheritance (upcast)
import Foreign.Ptr (Ptr)
import Foreign.C (CBool (..))
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_ShapeExtend_WireData.h hs_new_ShapeExtend_WireData_fromWireChainedAndManifold" rawFromWireChainedAndManifold :: Ptr TopoDS.Wire -> CBool -> CBool -> IO (Ptr WireData)

fromWireChainedAndManifold :: Ptr TopoDS.Wire -> Bool -> Bool -> Acquire (Ptr WireData)
fromWireChainedAndManifold theWire chained manifold = 
    mkAcquire (rawFromWireChainedAndManifold theWire (boolToCBool chained) (boolToCBool manifold)) (deleteWireData)

foreign import capi unsafe "hs_ShapeExtend_WireData.h hs_ShapeExtend_WireData_reverse" reverse :: Ptr WireData -> IO ()


foreign import capi unsafe "hs_ShapeExtend_WireData.h hs_ShapeExtend_WireData_wire" rawWire :: Ptr WireData -> IO (Ptr TopoDS.Wire)

wire :: Ptr WireData -> Acquire (Ptr TopoDS.Wire)
wire wireData = mkAcquire (rawWire wireData) (deleteShape . upcast)


foreign import capi unsafe "hs_ShapeExtend_WireData.h hs_ShapeExtend_WireData_wireAPIMake" rawWireAPIMake :: Ptr WireData -> IO (Ptr TopoDS.Wire)

wireAPIMake :: Ptr WireData -> Acquire (Ptr TopoDS.Wire)
wireAPIMake wireData = mkAcquire (rawWireAPIMake wireData) (deleteShape . upcast)

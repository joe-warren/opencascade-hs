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
import OpenCascade.Internal.Exception (wrapException)
import Foreign.Ptr (Ptr)
import Foreign.C (CBool (..), CInt)
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_ShapeExtend_WireData.h hs_new_ShapeExtend_WireData_fromWireChainedAndManifold" rawFromWireChainedAndManifold
    :: Ptr TopoDS.Wire
    -> CBool
    -> CBool
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr WireData)

fromWireChainedAndManifold :: Ptr TopoDS.Wire -> Bool -> Bool -> Acquire (Ptr WireData)
fromWireChainedAndManifold theWire chained manifold =
    mkAcquire (wrapException $ rawFromWireChainedAndManifold theWire (boolToCBool chained) (boolToCBool manifold)) (deleteWireData)

foreign import capi unsafe "hs_ShapeExtend_WireData.h hs_ShapeExtend_WireData_reverse" rawReverse
    :: Ptr WireData
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

reverse :: Ptr WireData -> IO ()
reverse wireData = wrapException $ rawReverse wireData


foreign import capi unsafe "hs_ShapeExtend_WireData.h hs_ShapeExtend_WireData_wire" rawWire
    :: Ptr WireData
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Wire)

wire :: Ptr WireData -> Acquire (Ptr TopoDS.Wire)
wire wireData = mkAcquire (wrapException $ rawWire wireData) (deleteShape . upcast)


foreign import capi unsafe "hs_ShapeExtend_WireData.h hs_ShapeExtend_WireData_wireAPIMake" rawWireAPIMake
    :: Ptr WireData
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Wire)

wireAPIMake :: Ptr WireData -> Acquire (Ptr TopoDS.Wire)
wireAPIMake wireData = mkAcquire (wrapException $ rawWireAPIMake wireData) (deleteShape . upcast)

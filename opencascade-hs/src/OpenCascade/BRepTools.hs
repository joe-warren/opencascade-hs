{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepTools
( module OpenCascade.BRepTools.Types
, outerWire
) where

import Data.Acquire (Acquire, mkAcquire)
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.BRepTools.Types
import Foreign.Ptr (Ptr)
import Foreign.C (CInt (..))
import OpenCascade.Inheritance (upcast)
import OpenCascade.Internal.Exception (wrapException)

foreign import capi unsafe "hs_BRepTools.h hs_BRepTools_OuterWire" rawOuterWire
    :: Ptr TopoDS.Face
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Wire)

outerWire :: Ptr TopoDS.Face -> Acquire (Ptr TopoDS.Wire)
outerWire face = mkAcquire (wrapException (rawOuterWire face)) (deleteShape . upcast)
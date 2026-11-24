{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepTools
( module OpenCascade.BRepTools.Types
, outerWire
, clean
) where

import Data.Acquire (Acquire, mkAcquire)
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.BRepTools.Types
import Foreign.Ptr (Ptr)
import Foreign.C (CBool (..), CInt (..))
import OpenCascade.Inheritance (upcast)
import OpenCascade.Internal.Exception (wrapException)
import OpenCascade.Internal.Bool (boolToCBool)

foreign import capi unsafe "hs_BRepTools.h hs_BRepTools_OuterWire" rawOuterWire
    :: Ptr TopoDS.Face
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Wire)

outerWire :: Ptr TopoDS.Face -> Acquire (Ptr TopoDS.Wire)
outerWire face = mkAcquire (wrapException (rawOuterWire face)) (deleteShape . upcast)

foreign import capi unsafe "hs_BRepTools.h hs_BRepTools_Clean" rawClean
    :: Ptr TopoDS.Shape
    -> CBool
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

clean :: Ptr TopoDS.Shape -> Bool -> IO ()
clean shape force = wrapException (rawClean shape (boolToCBool force))

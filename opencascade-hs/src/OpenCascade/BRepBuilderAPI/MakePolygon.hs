{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepBuilderAPI.MakePolygon
( from3Pnts 
) where

import qualified OpenCascade.GP as GP
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.Inheritance (upcast)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.C (CBool (..), CInt)
import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire, mkAcquire)
import OpenCascade.Internal.Bool (boolToCBool)

foreign import capi unsafe "hs_BRepBuilderAPI_MakePolygon.h hs_BRepBuilderAPI_MakePolygon_from3Pnts" rawFrom3Pnts
    :: Ptr GP.Pnt
    -> Ptr GP.Pnt
    -> Ptr GP.Pnt
    -> CBool
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Wire)

from3Pnts :: Ptr GP.Pnt -> Ptr GP.Pnt -> Ptr GP.Pnt -> Bool -> Acquire (Ptr TopoDS.Wire)
from3Pnts p1 p2 p3 close = mkAcquire (wrapException $ rawFrom3Pnts p1 p2 p3 (boolToCBool close)) (deleteShape . upcast)
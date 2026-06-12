{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepBuilderAPI.MakeVertex
( MakeVertex
, fromPnt
, vertex
) where

import OpenCascade.BRepBuilderAPI.Types (MakeVertex)
import OpenCascade.BRepBuilderAPI.Internal.Destructors (deleteMakeVertex)
import qualified OpenCascade.GP as GP
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.TopoDS.Internal.Destructors as TopoDS.Destructors
import OpenCascade.Internal.Exception (wrapException)
import Foreign.C (CInt)
import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire, mkAcquire)
import OpenCascade.Inheritance (upcast)

foreign import capi unsafe "hs_BRepBuilderAPI_MakeVertex.h hs_new_BRepBuilderAPI_MakeVertex_fromPnt" rawFromPnt
    :: Ptr GP.Pnt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr MakeVertex)

fromPnt :: Ptr GP.Pnt -> Acquire (Ptr MakeVertex)
fromPnt pnt = mkAcquire (wrapException $ rawFromPnt pnt) (deleteMakeVertex)


foreign import capi unsafe "hs_BRepBuilderAPI_MakeVertex.h hs_BRepBuilderAPI_MakeVertex_vertex" rawVertex
    :: Ptr MakeVertex
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Vertex)

vertex :: Ptr MakeVertex -> Acquire (Ptr TopoDS.Vertex)
vertex builder = mkAcquire (wrapException $ rawVertex builder) (TopoDS.Destructors.deleteShape . upcast)

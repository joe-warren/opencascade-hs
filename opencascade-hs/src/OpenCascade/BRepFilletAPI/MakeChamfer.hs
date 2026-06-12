{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepFilletAPI.MakeChamfer 
( MakeChamfer
, fromShape
, addEdge
, addEdgeWithDistance
, reset
, nbEdges
, edge
, remove
) where

import OpenCascade.BRepFilletAPI.Types (MakeChamfer)
import OpenCascade.BRepFilletAPI.Internal.Destructors (deleteMakeChamfer)
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.Inheritance (upcast)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.Ptr
import Foreign.C
import Data.Acquire
import Data.Coerce (coerce)


foreign import capi unsafe "hs_BRepFilletAPI_MakeChamfer.h hs_new_BRepFilletAPI_MakeChamfer_fromShape" rawFromShape
    :: Ptr TopoDS.Shape
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr MakeChamfer)

fromShape :: Ptr TopoDS.Shape  -> Acquire (Ptr MakeChamfer)
fromShape shape = mkAcquire (wrapException $ rawFromShape shape) deleteMakeChamfer

foreign import capi unsafe "hs_BRepFilletAPI_MakeChamfer.h hs_BRepFilletAPI_MakeChamfer_addEdge" rawAddEdge
    :: Ptr MakeChamfer
    -> Ptr TopoDS.Edge
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

addEdge :: Ptr MakeChamfer -> Ptr TopoDS.Edge -> IO ()
addEdge builder e = wrapException $ rawAddEdge builder e

foreign import capi unsafe "hs_BRepFilletAPI_MakeChamfer.h hs_BRepFilletAPI_MakeChamfer_addEdgeWithDistance" rawAddEdgeWithDistance
    :: Ptr MakeChamfer
    -> CDouble
    -> Ptr TopoDS.Edge
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

addEdgeWithDistance :: Ptr MakeChamfer -> Double -> Ptr TopoDS.Edge -> IO ()
addEdgeWithDistance builder d e = wrapException $ rawAddEdgeWithDistance builder (coerce d) e

foreign import capi unsafe "hs_BRepFilletAPI_MakeChamfer.h hs_BRepFilletAPI_MakeChamfer_reset" reset :: Ptr MakeChamfer -> IO ()

foreign import capi unsafe "hs_BRepFilletAPI_MakeChamfer.h hs_BRepFilletAPI_MakeChamfer_nbEdges" rawNbEdges :: Ptr MakeChamfer -> CInt -> IO CInt

nbEdges :: Ptr MakeChamfer -> Int -> IO Int
nbEdges builder index = fromIntegral <$> rawNbEdges builder (fromIntegral index)

foreign import capi unsafe "hs_BRepFilletAPI_MakeChamfer.h hs_BRepFilletAPI_MakeChamfer_edge" rawEdge
    :: Ptr MakeChamfer
    -> CInt
    -> CInt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Edge)

edge :: Ptr MakeChamfer -> Int -> Int -> Acquire (Ptr TopoDS.Edge)
edge builder contourIndex edgeIndex = mkAcquire (wrapException $ rawEdge builder (fromIntegral contourIndex) (fromIntegral edgeIndex)) (deleteShape . upcast)

foreign import capi unsafe "hs_BRepFilletAPI_MakeChamfer.h hs_BRepFilletAPI_MakeChamfer_remove" rawRemove
    :: Ptr MakeChamfer
    -> Ptr TopoDS.Edge
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

remove :: Ptr MakeChamfer -> Ptr TopoDS.Edge -> IO ()
remove builder e = wrapException $ rawRemove builder e
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
import Foreign.Ptr
import Foreign.C
import Data.Acquire
import Data.Coerce (coerce)


foreign import capi unsafe "hs_BRepFilletAPI_MakeChamfer.h hs_new_BRepFilletAPI_MakeChamfer_fromShape" rawFromShape :: Ptr TopoDS.Shape -> IO (Ptr MakeChamfer)

fromShape :: Ptr TopoDS.Shape  -> Acquire (Ptr MakeChamfer)
fromShape shape = mkAcquire (rawFromShape shape) deleteMakeChamfer

foreign import capi unsafe "hs_BRepFilletAPI_MakeChamfer.h hs_BRepFilletAPI_MakeChamfer_addEdge" addEdge :: Ptr MakeChamfer -> Ptr TopoDS.Edge -> IO ()

foreign import capi unsafe "hs_BRepFilletAPI_MakeChamfer.h hs_BRepFilletAPI_MakeChamfer_addEdgeWithDistance" rawAddEdgeWithDistance :: Ptr MakeChamfer -> CDouble -> Ptr TopoDS.Edge -> IO ()

addEdgeWithDistance :: Ptr MakeChamfer -> Double -> Ptr TopoDS.Edge -> IO ()
addEdgeWithDistance = coerce rawAddEdgeWithDistance

foreign import capi unsafe "hs_BRepFilletAPI_MakeChamfer.h hs_BRepFilletAPI_MakeChamfer_reset" reset :: Ptr MakeChamfer -> IO ()

foreign import capi unsafe "hs_BRepFilletAPI_MakeChamfer.h hs_BRepFilletAPI_MakeChamfer_nbEdges" rawNbEdges :: Ptr MakeChamfer -> CInt -> IO CInt

nbEdges :: Ptr MakeChamfer -> Int -> IO Int
nbEdges builder index = fromIntegral <$> rawNbEdges builder (fromIntegral index)

foreign import capi unsafe "hs_BRepFilletAPI_MakeChamfer.h hs_BRepFilletAPI_MakeChamfer_edge" rawEdge :: Ptr MakeChamfer -> CInt -> CInt -> IO (Ptr TopoDS.Edge)

edge :: Ptr MakeChamfer -> Int -> Int -> Acquire (Ptr TopoDS.Edge)
edge builder contourIndex edgeIndex = mkAcquire (rawEdge builder (fromIntegral contourIndex) (fromIntegral edgeIndex)) (deleteShape . upcast)

foreign import capi unsafe "hs_BRepFilletAPI_MakeChamfer.h hs_BRepFilletAPI_MakeChamfer_remove" remove :: Ptr MakeChamfer -> Ptr TopoDS.Edge -> IO ()
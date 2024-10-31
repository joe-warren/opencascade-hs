{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepFilletAPI.MakeFillet 
( MakeFillet
, fromShape
, addEdge
, addEdgeWithRadius
, addEdgeWithTwoRadiuses
, reset
, nbFaultyContours
, faultyContour
, nbEdges
, edge
, remove
) where

import OpenCascade.BRepFilletAPI.Types (MakeFillet)
import OpenCascade.BRepFilletAPI.Internal.Destructors (deleteMakeFillet)
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.Inheritance (upcast)
import Foreign.Ptr
import Foreign.C
import Data.Acquire
import Data.Coerce (coerce)


foreign import capi unsafe "hs_BRepFilletAPI_MakeFillet.h hs_new_BRepFilletAPI_MakeFillet_fromShape" rawFromShape :: Ptr TopoDS.Shape -> IO (Ptr MakeFillet)

fromShape :: Ptr TopoDS.Shape  -> Acquire (Ptr MakeFillet)
fromShape shape = mkAcquire (rawFromShape shape) deleteMakeFillet

foreign import capi unsafe "hs_BRepFilletAPI_MakeFillet.h hs_BRepFilletAPI_MakeFillet_addEdge" addEdge :: Ptr MakeFillet -> Ptr TopoDS.Edge -> IO ()

foreign import capi unsafe "hs_BRepFilletAPI_MakeFillet.h hs_BRepFilletAPI_MakeFillet_addEdgeWithRadius" rawAddEdgeWithRadius :: Ptr MakeFillet -> CDouble -> Ptr TopoDS.Edge -> IO ()

addEdgeWithRadius :: Ptr MakeFillet -> Double -> Ptr TopoDS.Edge -> IO ()
addEdgeWithRadius = coerce rawAddEdgeWithRadius

foreign import capi unsafe "hs_BRepFilletAPI_MakeFillet.h hs_BRepFilletAPI_MakeFillet_addEdgeWithTwoRadiuses" rawAddEdgeWithTwoRadiuses :: Ptr MakeFillet -> CDouble -> CDouble -> Ptr TopoDS.Edge -> IO ()

addEdgeWithTwoRadiuses :: Ptr MakeFillet -> Double -> Double -> Ptr TopoDS.Edge -> IO ()
addEdgeWithTwoRadiuses = coerce rawAddEdgeWithTwoRadiuses

foreign import capi unsafe "hs_BRepFilletAPI_MakeFillet.h hs_BRepFilletAPI_MakeFillet_reset" reset :: Ptr MakeFillet -> IO ()

foreign import capi unsafe "hs_BRepFilletAPI_MakeFillet.h hs_BRepFilletAPI_MakeFillet_nbFaultyContours" rawNbFaultyContours :: Ptr MakeFillet -> IO CInt

nbFaultyContours :: Ptr MakeFillet -> IO Int
nbFaultyContours = fmap fromIntegral . rawNbFaultyContours

foreign import capi unsafe "hs_BRepFilletAPI_MakeFillet.h hs_BRepFilletAPI_MakeFillet_faultyContour" rawFaultyContour :: Ptr MakeFillet -> CInt -> IO CInt

faultyContour :: Ptr MakeFillet -> Int -> IO Int
faultyContour builder index = fromIntegral <$> rawFaultyContour builder (fromIntegral index)

foreign import capi unsafe "hs_BRepFilletAPI_MakeFillet.h hs_BRepFilletAPI_MakeFillet_nbEdges" rawNbEdges :: Ptr MakeFillet -> CInt -> IO CInt

nbEdges :: Ptr MakeFillet -> Int -> IO Int
nbEdges builder index = fromIntegral <$> rawNbEdges builder (fromIntegral index)

foreign import capi unsafe "hs_BRepFilletAPI_MakeFillet.h hs_BRepFilletAPI_MakeFillet_edge" rawEdge :: Ptr MakeFillet -> CInt -> CInt -> IO (Ptr TopoDS.Edge)

edge :: Ptr MakeFillet -> Int -> Int -> Acquire (Ptr TopoDS.Edge)
edge builder contourIndex edgeIndex = mkAcquire (rawEdge builder (fromIntegral contourIndex) (fromIntegral edgeIndex)) (deleteShape . upcast)

foreign import capi unsafe "hs_BRepFilletAPI_MakeFillet.h hs_BRepFilletAPI_MakeFillet_remove" remove :: Ptr MakeFillet -> Ptr TopoDS.Edge -> IO ()
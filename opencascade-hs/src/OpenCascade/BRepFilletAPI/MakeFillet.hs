{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepFilletAPI.MakeFillet 
( MakeFillet
, fromShape
, addEdge
, addEdgeWithRadius
, addEdgeWithTwoRadiuses
) where

import OpenCascade.BRepFilletAPI.Types (MakeFillet)
import OpenCascade.BRepFilletAPI.Internal.Destructors (deleteMakeFillet)
import qualified OpenCascade.TopoDS as TopoDS
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
{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepBuilderAPI.MakeEdge
( fromVertices 
, fromPnts
, fromCurve 
, fromCurveAndParameters
, fromCurveAndVertices
, fromCurveAndPnts 
, fromCurveVerticesAndParameters
, fromCurvePntsAndParameters 
) where

import OpenCascade.TopoDS.Internal.Destructors as TopoDS.Destructors
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.GP as GP
import qualified OpenCascade.Geom as Geom
import OpenCascade.Handle
import Foreign.C
import Foreign.Ptr
import Data.Acquire 


-- fromVertices

foreign import capi unsafe "hs_BRepBuilderAPI_MakeEdge.h hs_BRepBuilderAPI_MakeEdge_fromVertices" rawFromVertices :: Ptr TopoDS.Vertex -> Ptr TopoDS.Vertex -> IO (Ptr TopoDS.Edge)

fromVertices :: Ptr TopoDS.Vertex -> Ptr TopoDS.Vertex -> Acquire (Ptr TopoDS.Edge)
fromVertices start end = mkAcquire (rawFromVertices start end) (TopoDS.Destructors.deleteShape . TopoDS.upcast)


-- fromPnts

foreign import capi unsafe "hs_BRepBuilderAPI_MakeEdge.h hs_BRepBuilderAPI_MakeEdge_fromPnts" rawFromPnts :: Ptr GP.Pnt -> Ptr GP.Pnt -> IO (Ptr TopoDS.Edge)

fromPnts :: Ptr GP.Pnt -> Ptr GP.Pnt -> Acquire (Ptr TopoDS.Edge)
fromPnts start end = mkAcquire (rawFromPnts start end) (TopoDS.Destructors.deleteShape . TopoDS.upcast)


-- fromCurve

foreign import capi unsafe "hs_BRepBuilderAPI_MakeEdge.h hs_BRepBuilderAPI_MakeEdge_fromCurve" rawFromCurve :: Ptr (Handle Geom.Curve) -> IO (Ptr TopoDS.Edge)

fromCurve :: Ptr (Handle Geom.Curve) -> Acquire (Ptr TopoDS.Edge)
fromCurve curve = mkAcquire (rawFromCurve curve) (TopoDS.Destructors.deleteShape . TopoDS.upcast)

-- fromCurveAndParameters

foreign import capi unsafe "hs_BRepBuilderAPI_MakeEdge.h hs_BRepBuilderAPI_MakeEdge_fromCurveAndParameters" rawFromCurveAndParameters :: Ptr (Handle Geom.Curve) -> CDouble -> CDouble -> IO (Ptr TopoDS.Edge)

fromCurveAndParameters :: Ptr (Handle Geom.Curve) -> Double -> Double -> Acquire (Ptr TopoDS.Edge)
fromCurveAndParameters curve p1 p2 = mkAcquire (rawFromCurveAndParameters curve (CDouble p1) (CDouble p2)) (TopoDS.Destructors.deleteShape . TopoDS.upcast)


-- fromCurveAndVertices

foreign import capi unsafe "hs_BRepBuilderAPI_MakeEdge.h hs_BRepBuilderAPI_MakeEdge_fromCurveAndVertices" rawFromCurveAndVertices :: Ptr (Handle Geom.Curve) -> Ptr TopoDS.Vertex -> Ptr TopoDS.Vertex -> IO (Ptr TopoDS.Edge)

fromCurveAndVertices :: Ptr (Handle Geom.Curve) -> Ptr TopoDS.Vertex -> Ptr TopoDS.Vertex -> Acquire (Ptr TopoDS.Edge)
fromCurveAndVertices curve v1 v2 = mkAcquire (rawFromCurveAndVertices curve v1 v2) (TopoDS.Destructors.deleteShape . TopoDS.upcast)


-- fromCurveAndPnts

foreign import capi unsafe "hs_BRepBuilderAPI_MakeEdge.h hs_BRepBuilderAPI_MakeEdge_fromCurveAndPnts" rawFromCurveAndPnts :: Ptr (Handle Geom.Curve) -> Ptr GP.Pnt -> Ptr GP.Pnt -> IO (Ptr TopoDS.Edge)

fromCurveAndPnts :: Ptr (Handle Geom.Curve) -> Ptr GP.Pnt -> Ptr GP.Pnt -> Acquire (Ptr TopoDS.Edge)
fromCurveAndPnts curve v1 v2 = mkAcquire (rawFromCurveAndPnts curve v1 v2) (TopoDS.Destructors.deleteShape . TopoDS.upcast)


-- fromCurveVerticesAndParameters

foreign import capi unsafe "hs_BRepBuilderAPI_MakeEdge.h hs_BRepBuilderAPI_MakeEdge_fromCurveVerticesAndParameters" rawFromCurveVerticesAndParameters :: Ptr (Handle Geom.Curve) -> Ptr TopoDS.Vertex -> Ptr TopoDS.Vertex -> CDouble -> CDouble -> IO (Ptr TopoDS.Edge)

fromCurveVerticesAndParameters :: Ptr (Handle Geom.Curve) -> Ptr TopoDS.Vertex -> Ptr TopoDS.Vertex -> Double -> Double -> Acquire (Ptr TopoDS.Edge)
fromCurveVerticesAndParameters curve v1 v2 p1 p2 = mkAcquire (rawFromCurveVerticesAndParameters curve v1 v2 (CDouble p1) (CDouble p2)) (TopoDS.Destructors.deleteShape . TopoDS.upcast)


-- fromCurvePntsAndParameters

foreign import capi unsafe "hs_BRepBuilderAPI_MakeEdge.h hs_BRepBuilderAPI_MakeEdge_fromCurvePntsAndParameters" rawFromCurvePntsAndParameters :: Ptr (Handle Geom.Curve) -> Ptr GP.Pnt -> Ptr GP.Pnt -> CDouble -> CDouble -> IO (Ptr TopoDS.Edge)

fromCurvePntsAndParameters :: Ptr (Handle Geom.Curve) -> Ptr GP.Pnt -> Ptr GP.Pnt -> Double -> Double -> Acquire (Ptr TopoDS.Edge)
fromCurvePntsAndParameters curve v1 v2 p1 p2 = mkAcquire (rawFromCurvePntsAndParameters curve v1 v2 (CDouble p1) (CDouble p2)) (TopoDS.Destructors.deleteShape . TopoDS.upcast)

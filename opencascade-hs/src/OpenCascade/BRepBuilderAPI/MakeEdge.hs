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

import OpenCascade.BRepBuilderAPI.Internal.Context
import OpenCascade.GP.Internal.Context (gpContext)
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import OpenCascade.Geom.Internal.Context (geomContext)
import OpenCascade.Handle.Internal.Context (handleContext)
import OpenCascade.TopoDS.Internal.Destructors as TopoDS.Destructors
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.GP as GP
import qualified OpenCascade.Geom as Geom
import OpenCascade.Handle
import OpenCascade.Inheritance
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.C
import Foreign.Ptr
import Data.Acquire

C.context (C.cppCtx <> gpContext <> topoDSContext <> handleContext <> geomContext <> brepBuilderAPIContext)

C.include "<BRepBuilderAPI_MakeEdge.hxx>"
C.include "<TopoDS_Edge.hxx>"
C.include "<Geom_Curve.hxx>" 
C.include "<Standard_Handle.hxx>"


-- fromVertices

fromVertices :: Ptr TopoDS.Vertex -> Ptr TopoDS.Vertex -> Acquire (Ptr TopoDS.Edge)
fromVertices startVertex endVertex = mkAcquire createEdge (TopoDS.Destructors.deleteShape . upcast)
  where
    createEdge = [C.throwBlock| TopoDS_Edge* {
      BRepBuilderAPI_MakeEdge maker(*$(TopoDS_Vertex* startVertex), *$(TopoDS_Vertex* endVertex));
      return new TopoDS_Edge(maker.Edge());
    } |]

-- fromPnts

fromPnts :: Ptr GP.Pnt -> Ptr GP.Pnt -> Acquire (Ptr TopoDS.Edge)
fromPnts startPnt endPnt = mkAcquire createEdge (TopoDS.Destructors.deleteShape . upcast)
  where
    createEdge = [C.throwBlock| TopoDS_Edge* {
      BRepBuilderAPI_MakeEdge maker(*$(gp_Pnt* startPnt), *$(gp_Pnt* endPnt));
      return new TopoDS_Edge(maker.Edge());
    } |]

-- fromCurve

fromCurve :: Ptr (Handle Geom.Curve) -> Acquire (Ptr TopoDS.Edge)
fromCurve curve = mkAcquire createEdge (TopoDS.Destructors.deleteShape . upcast)
  where
    createEdge = [C.throwBlock| TopoDS_Edge* {
      BRepBuilderAPI_MakeEdge maker(*$(opencascade::handle<Geom_Curve>* curve));
      return new TopoDS_Edge(maker.Edge());
    } |]

-- fromCurveAndParameters

fromCurveAndParameters :: Ptr (Handle Geom.Curve) -> Double -> Double -> Acquire (Ptr TopoDS.Edge)
fromCurveAndParameters curve p1 p2 = mkAcquire createEdge (TopoDS.Destructors.deleteShape . upcast)
  where
    createEdge = 
      let cP1 = realToFrac p1
          cP2 = realToFrac p2
      in [C.throwBlock| TopoDS_Edge* {
        BRepBuilderAPI_MakeEdge maker(*$(opencascade::handle<Geom_Curve>* curve), $(double cP1), $(double cP2));
        return new TopoDS_Edge(maker.Edge());
      } |]


-- fromCurveAndVertices

fromCurveAndVertices :: Ptr (Handle Geom.Curve) -> Ptr TopoDS.Vertex -> Ptr TopoDS.Vertex -> Acquire (Ptr TopoDS.Edge)
fromCurveAndVertices curve v1 v2 = mkAcquire createEdge (TopoDS.Destructors.deleteShape . upcast)
  where
    createEdge = [C.throwBlock| TopoDS_Edge* {
      BRepBuilderAPI_MakeEdge maker(*$(opencascade::handle<Geom_Curve>* curve), *$(TopoDS_Vertex* v1), *$(TopoDS_Vertex* v2));
      return new TopoDS_Edge(maker.Edge());
    } |]


-- fromCurveAndPnts

fromCurveAndPnts :: Ptr (Handle Geom.Curve) -> Ptr GP.Pnt -> Ptr GP.Pnt -> Acquire (Ptr TopoDS.Edge)
fromCurveAndPnts curve v1 v2 = mkAcquire createEdge (TopoDS.Destructors.deleteShape . upcast)
  where
    createEdge = [C.throwBlock| TopoDS_Edge* {
      BRepBuilderAPI_MakeEdge maker(*$(opencascade::handle<Geom_Curve>* curve), *$(gp_Pnt* v1), *$(gp_Pnt* v2));
      return new TopoDS_Edge(maker.Edge());
    } |]


-- fromCurveVerticesAndParameters

fromCurveVerticesAndParameters :: Ptr (Handle Geom.Curve) -> Ptr TopoDS.Vertex -> Ptr TopoDS.Vertex -> Double -> Double -> Acquire (Ptr TopoDS.Edge)
fromCurveVerticesAndParameters curve v1 v2 p1 p2 = mkAcquire createEdge (TopoDS.Destructors.deleteShape . upcast)
  where
    createEdge = 
      let cP1 = realToFrac p1
          cP2 = realToFrac p2
      in [C.throwBlock| TopoDS_Edge* {
        BRepBuilderAPI_MakeEdge maker(*$(opencascade::handle<Geom_Curve>* curve), *$(TopoDS_Vertex* v1), *$(TopoDS_Vertex* v2), $(double cP1), $(double cP2));
        return new TopoDS_Edge(maker.Edge());
      } |]


-- fromCurvePntsAndParameters

fromCurvePntsAndParameters :: Ptr (Handle Geom.Curve) -> Ptr GP.Pnt -> Ptr GP.Pnt -> Double -> Double -> Acquire (Ptr TopoDS.Edge)
fromCurvePntsAndParameters curve v1 v2 p1 p2 = mkAcquire createEdge (TopoDS.Destructors.deleteShape . upcast)
  where
    createEdge = 
      let cP1 = realToFrac p1
          cP2 = realToFrac p2
      in [C.throwBlock| TopoDS_Edge* {
        BRepBuilderAPI_MakeEdge maker(*$(opencascade::handle<Geom_Curve>* curve), *$(gp_Pnt* v1), *$(gp_Pnt* v2), $(double cP1), $(double cP2));
        return new TopoDS_Edge(maker.Edge());
      } |]

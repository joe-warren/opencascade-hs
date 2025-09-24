module OpenCascade.BRep.Tool
( curve
, curveParamFirst
, curveParamLast
, pnt
, triangulation
) where

import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import OpenCascade.TopLoc.Internal.Context (topLocContext)
import OpenCascade.GP.Internal.Context (gpContext)
import OpenCascade.Geom.Internal.Context (geomContext)
import OpenCascade.Handle.Internal.Context (handleContext)
import OpenCascade.Poly.Internal.Context (polyContext)
import qualified OpenCascade.Geom as Geom
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.GP as GP
import qualified OpenCascade.TopLoc.Types as TopLoc
import qualified OpenCascade.Poly.Types as Poly
import OpenCascade.Poly.Internal.Destructors (deleteHandleTriangulation)
import OpenCascade.Handle (Handle)
import OpenCascade.Geom.Internal.Destructors (deleteHandleCurve)
import OpenCascade.GP.Internal.Destructors (deletePnt)
import Foreign.Ptr
import Foreign.C
import Data.Acquire (Acquire, mkAcquire)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> topoDSContext <> topLocContext <> gpContext <> geomContext <> handleContext <> polyContext)

C.include "<BRep_Tool.hxx>"
C.include "<TopoDS_Edge.hxx>"
C.include "<TopoDS_Vertex.hxx>"
C.include "<TopoDS_Face.hxx>"
C.include "<TopLoc_Location.hxx>"
C.include "<Geom_Curve.hxx>"
C.include "<Poly_Triangulation.hxx>"
C.include "<Standard_Handle.hxx>"
C.include "<gp_Pnt.hxx>"

curve :: Ptr TopoDS.Edge -> Acquire (Ptr (Handle Geom.Curve))
curve edge =
  let createCurve = [C.throwBlock| opencascade::handle<Geom_Curve>* {
        double first, last;
        return new opencascade::handle<Geom_Curve>(BRep_Tool::Curve(*$(TopoDS_Edge* edge), first, last));
      } |]
  in mkAcquire createCurve deleteHandleCurve

curveParamFirst :: Ptr TopoDS.Edge -> IO Double 
curveParamFirst edge = do
  result <- [C.throwBlock| double {
    double first, last;
    BRep_Tool::Curve(*$(TopoDS_Edge* edge), first, last);
    return first;
  } |]
  return (realToFrac result)

curveParamLast :: Ptr TopoDS.Edge -> IO Double 
curveParamLast edge = do
  result <- [C.throwBlock| double {
    double first, last;
    BRep_Tool::Curve(*$(TopoDS_Edge* edge), first, last);
    return last;
  } |]
  return (realToFrac result)

pnt :: Ptr TopoDS.Vertex -> Acquire (Ptr GP.Pnt)
pnt vertex =
  let createPnt = [C.throwBlock| gp_Pnt* {
        return new gp_Pnt(BRep_Tool::Pnt(*$(TopoDS_Vertex* vertex)));
      } |]
  in mkAcquire createPnt deletePnt

triangulation :: Ptr TopoDS.Face -> Ptr TopLoc.Location -> Acquire (Ptr (Handle Poly.Triangulation))
triangulation face loc =
  let createTriangulation = [C.throwBlock| opencascade::handle<Poly_Triangulation>* {
        return new opencascade::handle<Poly_Triangulation>(BRep_Tool::Triangulation(*$(TopoDS_Face* face), *$(TopLoc_Location* loc)));
      } |]
  in mkAcquire createTriangulation deleteHandleTriangulation
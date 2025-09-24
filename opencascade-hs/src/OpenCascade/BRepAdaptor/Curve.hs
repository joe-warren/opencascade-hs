module OpenCascade.BRepAdaptor.Curve
( Curve
, fromEdge
, curveType
, bezier
, bspline
, curve
, firstParameter
, lastParameter
) where

import OpenCascade.BRepAdaptor.Internal.Context
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import OpenCascade.Geom.Internal.Context (geomContext)
import OpenCascade.Handle.Internal.Context (handleContext)
import OpenCascade.BRepAdaptor.Types (Curve)
import OpenCascade.BRepAdaptor.Internal.Destructors (deleteCurve)
import OpenCascade.Geom.Internal.Destructors (deleteHandleBezierCurve, deleteHandleBSplineCurve)
import qualified OpenCascade.Geom.Types as Geom
import OpenCascade.GeomAbs.CurveType (CurveType)
import qualified OpenCascade.TopoDS as TopoDS
import Foreign.Ptr (Ptr)
import Foreign.C (CInt (..), CDouble (..))
import Data.Acquire (Acquire, mkAcquire)
import OpenCascade.Handle (Handle)
import qualified OpenCascade.GeomAdaptor.Types as GeomAdaptor
import qualified OpenCascade.GeomAdaptor.Internal.Destructors as GeomAdaptor.Destructors
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> topoDSContext <> geomContext <> handleContext <> brepAdaptorContext <> C.cppTypePairs [("GeomAdaptor_Curve", [t| GeomAdaptor.Curve |])])

C.include "<BRepAdaptor_Curve.hxx>"
C.include "<TopoDS_Edge.hxx>"
C.include "<Geom_Curve.hxx>"
C.include "<Geom_BezierCurve.hxx>"
C.include "<Geom_BSplineCurve.hxx>"
C.include "<GeomAdaptor_Curve.hxx>"
C.include "<GeomAbs_CurveType.hxx>"
C.include "<Standard_Handle.hxx>"

fromEdge :: Ptr TopoDS.Edge -> Acquire (Ptr Curve)
fromEdge edge =
  let createCurve = [C.throwBlock| BRepAdaptor_Curve* {
        return new BRepAdaptor_Curve(*$(TopoDS_Edge* edge));
      } |]
  in mkAcquire createCurve deleteCurve

curveType :: Ptr Curve -> IO CurveType
curveType adaptorCurve = do
  result <- [C.throwBlock| int {
    return static_cast<int>($(BRepAdaptor_Curve* adaptorCurve)->GetType());
  } |]
  return (toEnum $ fromIntegral result)

bezier :: Ptr Curve -> Acquire (Ptr (Handle Geom.BezierCurve))
bezier adaptorCurve =
  let createBezier = [C.throwBlock| opencascade::handle<Geom_BezierCurve>* {
        return new opencascade::handle<Geom_BezierCurve>($(BRepAdaptor_Curve* adaptorCurve)->Bezier());
      } |]
  in mkAcquire createBezier deleteHandleBezierCurve

bspline :: Ptr Curve -> Acquire (Ptr (Handle Geom.BSplineCurve))
bspline adaptorCurve =
  let createBSpline = [C.throwBlock| opencascade::handle<Geom_BSplineCurve>* {
        return new opencascade::handle<Geom_BSplineCurve>($(BRepAdaptor_Curve* adaptorCurve)->BSpline());
      } |]
  in mkAcquire createBSpline deleteHandleBSplineCurve

curve :: Ptr Curve -> Acquire (Ptr GeomAdaptor.Curve)
curve adaptorCurve =
  let createGeomCurve = [C.throwBlock| GeomAdaptor_Curve* {
        return new GeomAdaptor_Curve($(BRepAdaptor_Curve* adaptorCurve)->Curve());
      } |]
  in mkAcquire createGeomCurve GeomAdaptor.Destructors.deleteCurve

firstParameter :: Ptr Curve -> IO Double
firstParameter adaptorCurve = do
  result <- [C.throwBlock| double {
    return $(BRepAdaptor_Curve* adaptorCurve)->FirstParameter();
  } |]
  return (realToFrac result)

lastParameter :: Ptr Curve -> IO Double
lastParameter adaptorCurve = do
  result <- [C.throwBlock| double {
    return $(BRepAdaptor_Curve* adaptorCurve)->LastParameter();
  } |]
  return (realToFrac result)
module OpenCascade.Geom.Internal.Destructors 
( deleteHandleCurve
, deleteHandleTrimmedCurve
, deleteHandleBezierCurve
, deleteHandleBSplineCurve
, deleteBezierCurve
) where

import OpenCascade.Geom.Internal.Context
import OpenCascade.Handle.Internal.Context (handleContext)
import OpenCascade.Geom.Types
import OpenCascade.Handle
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr

C.context (C.cppCtx <> handleContext <> geomContext)

C.include "<Standard_Handle.hxx>"
C.include "<Geom_Curve.hxx>"
C.include "<Geom_TrimmedCurve.hxx>"
C.include "<Geom_BezierCurve.hxx>"
C.include "<Geom_BSplineCurve.hxx>"

deleteHandleCurve :: Ptr (Handle Curve) -> IO ()
deleteHandleCurve handlePtr = [C.throwBlock| void {
  delete $(opencascade::handle<Geom_Curve>* handlePtr);
} |]

deleteHandleTrimmedCurve :: Ptr (Handle TrimmedCurve) -> IO ()
deleteHandleTrimmedCurve handlePtr = [C.throwBlock| void {
  delete $(opencascade::handle<Geom_TrimmedCurve>* handlePtr);
} |]

deleteHandleBezierCurve :: Ptr (Handle BezierCurve) -> IO ()
deleteHandleBezierCurve handlePtr = [C.throwBlock| void {
  delete $(opencascade::handle<Geom_BezierCurve>* handlePtr);
} |]

deleteHandleBSplineCurve :: Ptr (Handle BSplineCurve) -> IO ()
deleteHandleBSplineCurve handlePtr = [C.throwBlock| void {
  delete $(opencascade::handle<Geom_BSplineCurve>* handlePtr);
} |]

deleteBezierCurve :: Ptr BezierCurve -> IO ()
deleteBezierCurve curvePtr = [C.throwBlock| void {
  delete $(Geom_BezierCurve* curvePtr);
} |]

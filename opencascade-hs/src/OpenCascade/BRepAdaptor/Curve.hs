{-# LANGUAGE CApiFFI #-} 
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
import Data.Coerce (coerce)
import qualified OpenCascade.GeomAdaptor.Types as GeomAdaptor
import qualified OpenCascade.GeomAdaptor.Internal.Destructors as GeomAdaptor.Destructors

foreign import capi unsafe "hs_BRepAdaptor_Curve.h hs_new_BRepAdaptor_Curve_fromEdge" rawFromEdge :: Ptr TopoDS.Edge -> IO (Ptr Curve)

fromEdge :: Ptr TopoDS.Edge -> Acquire (Ptr Curve)
fromEdge e = mkAcquire (rawFromEdge e) deleteCurve


foreign import capi unsafe "hs_BRepAdaptor_Curve.h hs_BRepAdaptor_Curve_curveType" rawCurveType :: Ptr Curve -> IO (CInt)

curveType :: Ptr Curve -> IO CurveType
curveType c = toEnum . fromIntegral <$> rawCurveType c

foreign import capi unsafe "hs_BRepAdaptor_Curve.h hs_BRepAdaptor_Curve_bezier" rawBezier :: Ptr Curve -> IO (Ptr (Handle (Geom.BezierCurve)))

bezier :: Ptr Curve -> Acquire (Ptr (Handle Geom.BezierCurve))
bezier theCurve = mkAcquire (rawBezier theCurve) deleteHandleBezierCurve

foreign import capi unsafe "hs_BRepAdaptor_Curve.h hs_BRepAdaptor_Curve_bspline" rawBSpline :: Ptr Curve -> IO (Ptr (Handle (Geom.BSplineCurve)))

bspline :: Ptr Curve -> Acquire (Ptr (Handle Geom.BSplineCurve))
bspline theCurve = mkAcquire (rawBSpline theCurve) deleteHandleBSplineCurve


foreign import capi unsafe "hs_BRepAdaptor_Curve.h hs_BRepAdaptor_Curve_curve" rawCurve :: Ptr Curve -> IO (Ptr GeomAdaptor.Curve)

curve :: Ptr Curve -> Acquire (Ptr GeomAdaptor.Curve)
curve theCurve = mkAcquire (rawCurve theCurve) GeomAdaptor.Destructors.deleteCurve

foreign import capi unsafe "hs_BRepAdaptor_Curve.h hs_BRepAdaptor_Curve_firstParameter" rawFirstParameter :: Ptr Curve -> IO CDouble

firstParameter :: Ptr Curve -> IO Double
firstParameter = coerce rawFirstParameter

foreign import capi unsafe "hs_BRepAdaptor_Curve.h hs_BRepAdaptor_Curve_lastParameter" rawLastParameter :: Ptr Curve -> IO CDouble

lastParameter :: Ptr Curve -> IO Double
lastParameter = coerce rawLastParameter
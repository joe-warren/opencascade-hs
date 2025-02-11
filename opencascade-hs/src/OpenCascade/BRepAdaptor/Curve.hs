{-# LANGUAGE CApiFFI #-} 
module OpenCascade.BRepAdaptor.Curve
( Curve
, fromEdge
, curveType
) where
 
import OpenCascade.BRepAdaptor.Types (Curve)
import OpenCascade.BRepAdaptor.Internal.Destructors (deleteCurve)
import OpenCascade.GeomAbs.CurveType (CurveType)
import qualified OpenCascade.TopoDS as TopoDS
import Foreign.Ptr (Ptr)
import Foreign.C (CInt (..))
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_BRepAdaptor_Curve.h hs_new_BRepAdaptor_Curve_fromEdge" rawFromEdge :: Ptr TopoDS.Edge -> IO (Ptr Curve)

fromEdge :: Ptr TopoDS.Edge -> Acquire (Ptr Curve)
fromEdge e = mkAcquire (rawFromEdge e) deleteCurve


foreign import capi unsafe "hs_BRepAdaptor_Curve.h hs_BRepAdaptor_Curve_curveType" rawCurveType :: Ptr Curve -> IO (CInt)

curveType :: Ptr Curve -> IO CurveType
curveType c = toEnum . fromIntegral <$> rawCurveType c

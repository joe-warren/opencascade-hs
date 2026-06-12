{-# LANGUAGE CApiFFI #-}
module OpenCascade.GeomConvert.ApproxCurve
( ApproxCurve
, fromCurveToleranceOrderSegmentsAndDegree
, curve
, isDone
, hasResult
) where 

import OpenCascade.GeomConvert.Types (ApproxCurve)
import OpenCascade.GeomConvert.Internal.Destructors (deleteApproxCurve)
import OpenCascade.Geom.Internal.Destructors (deleteHandleBSplineCurve)
import qualified OpenCascade.GeomAbs.Shape as GeomAbs.Shape
import Foreign.Ptr (Ptr)
import Foreign.C (CInt (..), CDouble (..), CBool (..))
import Data.Coerce (coerce)
import Data.Acquire (Acquire, mkAcquire)
import OpenCascade.Handle (Handle)
import OpenCascade.Geom.Types as Geom
import OpenCascade.Internal.Bool (cBoolToBool)
import OpenCascade.Internal.Exception (wrapException)

foreign import capi unsafe "hs_GeomConvert_ApproxCurve.h hs_new_GeomConvert_ApproxCurve_fromCurveToleranceOrderSegmentsAndDegree" rawFromCurveToleranceOrderSegmentsAndDegree
    :: Ptr (Handle Geom.Curve)
    -> CDouble
    -> CInt
    -> CInt
    -> CInt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr ApproxCurve)

fromCurveToleranceOrderSegmentsAndDegree :: Ptr (Handle Geom.Curve) -> Double -> GeomAbs.Shape.Shape -> Int -> Int -> Acquire (Ptr ApproxCurve)
fromCurveToleranceOrderSegmentsAndDegree theCurve tolerance order maxSegments maxDegree =
    mkAcquire
        (wrapException $ rawFromCurveToleranceOrderSegmentsAndDegree
            theCurve
            (coerce tolerance)
            (fromIntegral . fromEnum $ order)
            (fromIntegral maxSegments)
            (fromIntegral maxDegree))
        deleteApproxCurve


foreign import capi unsafe "hs_GeomConvert_ApproxCurve.h hs_GeomConvert_ApproxCurve_curve" rawCurve
    :: Ptr ApproxCurve
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr (Handle Geom.BSplineCurve))

curve :: Ptr ApproxCurve -> Acquire (Ptr (Handle Geom.BSplineCurve))
curve approxCurve = mkAcquire (wrapException $ rawCurve approxCurve) deleteHandleBSplineCurve

foreign import capi unsafe "hs_GeomConvert_ApproxCurve.h hs_GeomConvert_ApproxCurve_isDone" rawIsDone :: Ptr ApproxCurve -> IO (CBool)

isDone :: Ptr ApproxCurve -> IO Bool
isDone approxCurve = cBoolToBool <$> rawIsDone approxCurve


foreign import capi unsafe "hs_GeomConvert_ApproxCurve.h hs_GeomConvert_ApproxCurve_hasResult" rawHasResult :: Ptr ApproxCurve -> IO (CBool)

hasResult :: Ptr ApproxCurve -> IO Bool
hasResult approxCurve = cBoolToBool <$> rawHasResult approxCurve
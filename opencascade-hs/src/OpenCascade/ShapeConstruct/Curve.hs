{-# LANGUAGE CApiFFI #-}
module OpenCascade.ShapeConstruct.Curve
( Curve
, new
, convertToBSpline
) where

import OpenCascade.ShapeConstruct.Types (Curve)
import OpenCascade.ShapeConstruct.Internal.Destructors (deleteCurve)
import qualified OpenCascade.Geom.Types as Geom
import OpenCascade.Geom.Internal.Destructors (deleteHandleBSplineCurve)
import OpenCascade.Handle (Handle)
import Foreign.Ptr
import Foreign.C (CDouble (..))
import Data.Acquire (Acquire, mkAcquire)
import Data.Coerce (coerce)

foreign import capi unsafe "hs_ShapeConstruct_Curve.h hs_new_ShapeConstruct_Curve" rawNew :: IO (Ptr Curve)

new :: Acquire (Ptr Curve)
new = mkAcquire rawNew deleteCurve

foreign import capi unsafe "hs_ShapeConstruct_Curve.h hs_ShapeConstruct_Curve_convertToBSpline" rawConvertToBSpline :: Ptr Curve -> Ptr (Handle Geom.Curve) -> CDouble -> CDouble -> CDouble -> IO (Ptr (Handle Geom.BSplineCurve))

convertToBSpline :: Ptr Curve -> Ptr (Handle Geom.Curve) -> Double -> Double -> Double -> Acquire (Ptr (Handle Geom.BSplineCurve))
convertToBSpline shapeConstruct handleCurve firstParam lastParam precision = 
    mkAcquire (rawConvertToBSpline shapeConstruct handleCurve (coerce firstParam) (coerce lastParam) (coerce precision)) deleteHandleBSplineCurve


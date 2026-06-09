{-# LANGUAGE CApiFFI #-}
module OpenCascade.GeomAdaptor.Curve
( Curve
, fromHandle
, firstParameter
, lastParameter
, curve
) where

import OpenCascade.GeomAdaptor.Types (Curve)
import qualified OpenCascade.Geom.Types as Geom
import qualified OpenCascade.Geom.Internal.Destructors as Geom.Destructors
import OpenCascade.GeomAdaptor.Internal.Destructors (deleteCurve)
import OpenCascade.Handle (Handle)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.C (CDouble (..), CInt)
import Data.Coerce (coerce)
import Foreign.Ptr
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_GeomAdaptor_Curve.h hs_new_GeomAdaptor_Curve_fromHandle" rawFromHandle
    :: Ptr (Handle Geom.Curve)
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr Curve)

fromHandle :: Ptr (Handle Geom.Curve) -> Acquire (Ptr Curve)
fromHandle h = mkAcquire (wrapException $ rawFromHandle h) deleteCurve


foreign import capi unsafe "hs_GeomAdaptor_Curve.h hs_GeomAdaptor_Curve_curve" rawCurve
    :: Ptr Curve
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr (Handle Geom.Curve))

curve :: Ptr Curve -> Acquire (Ptr (Handle Geom.Curve))
curve adaptor = mkAcquire (wrapException $ rawCurve adaptor) Geom.Destructors.deleteHandleCurve

foreign import capi unsafe "hs_GeomAdaptor_Curve.h hs_GeomAdaptor_Curve_firstParameter" rawFirstParameter :: Ptr Curve-> IO (CDouble)

firstParameter :: Ptr Curve -> IO Double
firstParameter = coerce rawFirstParameter

foreign import capi unsafe "hs_GeomAdaptor_Curve.h hs_GeomAdaptor_Curve_lastParameter" rawLastParameter :: Ptr Curve -> IO (CDouble)

lastParameter :: Ptr Curve -> IO Double
lastParameter = coerce rawLastParameter
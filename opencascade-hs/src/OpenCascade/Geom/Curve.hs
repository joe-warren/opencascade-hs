{-# LANGUAGE CApiFFI #-}
module OpenCascade.Geom.Curve 
( value
, firstParameter
, lastParameter
, dn
, reversedParameter
, reversed
) where
import Foreign.Ptr
import Foreign.C
import Data.Coerce
import Data.Acquire
import OpenCascade.Geom.Types (Curve)
import OpenCascade.GP (Pnt, Vec)
import OpenCascade.GP.Internal.Destructors (deletePnt, deleteVec)
import OpenCascade.Geom.Internal.Destructors (deleteHandleCurve)
import OpenCascade.Handle (Handle)
import OpenCascade.Internal.Exception (wrapException)

foreign import capi unsafe "hs_Geom_Curve.h hs_Geom_Curve_value" rawValue
    :: Ptr (Handle Curve)
    -> CDouble
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr Pnt)

value :: Ptr (Handle Curve) -> Double -> Acquire (Ptr Pnt)
value curve u = mkAcquire (wrapException $ rawValue curve (coerce u)) deletePnt

foreign import capi unsafe "hs_Geom_Curve.h hs_Geom_Curve_firstParameter" rawFirstParameter :: Ptr (Handle Curve)-> IO (CDouble)

firstParameter :: Ptr (Handle Curve) -> IO Double
firstParameter = coerce rawFirstParameter

foreign import capi unsafe "hs_Geom_Curve.h hs_Geom_Curve_lastParameter" rawLastParameter :: Ptr (Handle Curve)-> IO (CDouble)

lastParameter :: Ptr (Handle Curve) -> IO Double
lastParameter = coerce rawLastParameter

foreign import capi unsafe "hs_Geom_Curve.h hs_Geom_Curve_dn" rawDN
    :: Ptr (Handle Curve)
    -> CDouble
    -> CInt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr Vec)

dn :: Ptr (Handle Curve) -> Double -> Int -> Acquire (Ptr Vec)
dn curve u n = mkAcquire (wrapException $ rawDN curve (coerce u) (fromIntegral n)) deleteVec

foreign import capi unsafe "hs_Geom_Curve.h hs_Geom_Curve_reversedParameter" rawReversedParameter
    :: Ptr (Handle Curve)
    -> CDouble
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO CDouble

reversedParameter :: Ptr (Handle Curve) -> Double -> IO Double
reversedParameter curve parameter = coerce <$> wrapException (rawReversedParameter curve (coerce parameter))

foreign import capi unsafe "hs_Geom_Curve.h hs_Geom_Curve_reversed" rawReversed
    :: Ptr (Handle Curve)
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr (Handle Curve))

reversed :: Ptr (Handle Curve) -> Acquire (Ptr (Handle Curve))
reversed c = mkAcquire (wrapException $ rawReversed c) deleteHandleCurve

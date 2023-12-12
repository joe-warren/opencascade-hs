{-# LANGUAGE CApiFFI #-}
module OpenCascade.Geom.Curve 
( value
, dn
) where
import Foreign.Ptr
import Foreign.C
import Data.Coerce
import Data.Acquire
import OpenCascade.Geom.Types (Curve)
import OpenCascade.GP (Pnt, Vec)
import OpenCascade.GP.Internal.Destructors (deletePnt, deleteVec)
import OpenCascade.Handle (Handle)
foreign import capi unsafe "hs_Geom_Curve.h hs_Geom_Curve_value" rawValue :: Ptr (Handle Curve) -> CDouble -> IO(Ptr Pnt)

value :: Ptr (Handle Curve) -> Double -> Acquire (Ptr Pnt)
value curve u = mkAcquire (rawValue curve (coerce u)) deletePnt

foreign import capi unsafe "hs_Geom_Curve.h hs_Geom_Curve_dn" rawDN :: Ptr (Handle Curve) -> CDouble -> CInt -> IO (Ptr Vec)

dn :: Ptr (Handle Curve) -> Double -> Int -> Acquire (Ptr Vec)
dn curve u n = mkAcquire (rawDN curve (coerce u) (fromIntegral n)) deleteVec
{-# LANGUAGE CApiFFI #-}
module OpenCascade.Geom.Curve 
( value
) where
import Foreign.Ptr
import Foreign.C
import Data.Coerce
import Data.Acquire
import OpenCascade.Geom.Types (Curve)
import OpenCascade.GP (Pnt)
import OpenCascade.GP.Internal.Destructors (deletePnt)
import OpenCascade.Handle (Handle)
foreign import capi unsafe "hs_Geom_Curve.h hs_Geom_Curve_value" rawValue :: Ptr (Handle Curve) -> CDouble -> IO(Ptr Pnt)

value :: Ptr (Handle Curve) -> Double -> Acquire (Ptr Pnt)
value curve u = mkAcquire (rawValue curve (coerce u)) deletePnt

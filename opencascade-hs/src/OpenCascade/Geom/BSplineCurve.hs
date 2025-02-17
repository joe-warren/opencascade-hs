{-# LANGUAGE CApiFFI #-}
module OpenCascade.Geom.BSplineCurve 
( toHandle
) where
import Foreign.Ptr
import Data.Acquire
import OpenCascade.Geom.Types (BSplineCurve)
import OpenCascade.Geom.Internal.Destructors (deleteHandleBSplineCurve)
import OpenCascade.Handle (Handle)

foreign import capi unsafe "hs_Geom_BSplineCurve.h hs_Geom_BSplineCurve_toHandle" rawToHandle :: Ptr BSplineCurve -> IO (Ptr (Handle BSplineCurve))

toHandle :: Ptr BSplineCurve -> Acquire (Ptr (Handle BSplineCurve))
toHandle curve = mkAcquire (rawToHandle curve) deleteHandleBSplineCurve

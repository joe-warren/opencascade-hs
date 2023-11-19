{-# LANGUAGE CApiFFI #-}
module OpenCascade.Geom.BezierCurve 
( fromPnts
, toHandle
) where
import Foreign.Ptr
import Data.Acquire
import OpenCascade.Geom.Types (BezierCurve)
import OpenCascade.Geom.Internal.Destructors (deleteBezierCurve, deleteHandleBezierCurve)
import OpenCascade.GP (Pnt(..))
import OpenCascade.NCollection (Array1)
import OpenCascade.Handle (Handle)
foreign import capi unsafe "hs_Geom_BezierCurve.h hs_new_Geom_BezierCurve_fromPnts" rawFromPnts :: Ptr (Array1 Pnt) -> IO(Ptr BezierCurve)

fromPnts :: Ptr (Array1 Pnt) -> Acquire (Ptr BezierCurve)
fromPnts arr = mkAcquire (rawFromPnts arr) (deleteBezierCurve)

foreign import capi unsafe "hs_Geom_BezierCurve.h hs_Geom_BezierCurve_toHandle" rawToHandle :: Ptr BezierCurve -> IO (Ptr (Handle BezierCurve))

toHandle :: Ptr BezierCurve -> Acquire (Ptr (Handle BezierCurve))
toHandle curve = mkAcquire (rawToHandle curve) deleteHandleBezierCurve




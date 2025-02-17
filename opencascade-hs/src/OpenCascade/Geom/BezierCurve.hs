{-# LANGUAGE CApiFFI #-}
module OpenCascade.Geom.BezierCurve 
( fromPnts
, toHandle
, nbPoles
, pole
, isRational
) where
import Foreign.Ptr
import Foreign.C (CInt (..), CBool (..))
import Data.Acquire
import OpenCascade.Geom.Types (BezierCurve)
import OpenCascade.Geom.Internal.Destructors (deleteBezierCurve, deleteHandleBezierCurve)
import OpenCascade.GP.Internal.Destructors (deletePnt)
import OpenCascade.GP (Pnt)
import OpenCascade.NCollection (Array1)
import OpenCascade.Handle (Handle)
import OpenCascade.Internal.Bool (cBoolToBool)
foreign import capi unsafe "hs_Geom_BezierCurve.h hs_new_Geom_BezierCurve_fromPnts" rawFromPnts :: Ptr (Array1 Pnt) -> IO(Ptr BezierCurve)

fromPnts :: Ptr (Array1 Pnt) -> Acquire (Ptr BezierCurve)
fromPnts arr = mkAcquire (rawFromPnts arr) (deleteBezierCurve)

foreign import capi unsafe "hs_Geom_BezierCurve.h hs_Geom_BezierCurve_toHandle" rawToHandle :: Ptr BezierCurve -> IO (Ptr (Handle BezierCurve))

toHandle :: Ptr BezierCurve -> Acquire (Ptr (Handle BezierCurve))
toHandle curve = mkAcquire (rawToHandle curve) deleteHandleBezierCurve


foreign import capi unsafe "hs_Geom_BezierCurve.h hs_Geom_BezierCurve_nbPoles" rawNbPoles :: Ptr (Handle BezierCurve) -> IO (CInt)

nbPoles :: Ptr (Handle (BezierCurve)) -> IO Int 
nbPoles h = fromIntegral <$> rawNbPoles h

foreign import capi unsafe "hs_Geom_BezierCurve.h hs_Geom_BezierCurve_pole" rawPole :: Ptr (Handle BezierCurve) -> CInt -> IO (Ptr Pnt)

pole :: Ptr (Handle BezierCurve) -> Int -> Acquire (Ptr Pnt)
pole h n = mkAcquire (rawPole h (fromIntegral n)) deletePnt

foreign import capi unsafe "hs_Geom_BezierCurve.h hs_Geom_BezierCurve_isRational" rawIsRational :: Ptr (Handle BezierCurve) -> IO (CBool)

isRational :: Ptr (Handle (BezierCurve)) -> IO Bool
isRational h = cBoolToBool <$> rawIsRational h

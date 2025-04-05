{-# LANGUAGE CApiFFI #-}
module OpenCascade.Geom.BSplineCurve 
( toHandle
, nbPoles
, pole
, isRational
, segment
) where
import Foreign.Ptr
import Foreign.C (CInt (..), CBool (..), CDouble (..))
import Data.Acquire
import OpenCascade.Geom.Types (BSplineCurve)
import OpenCascade.Geom.Internal.Destructors (deleteHandleBSplineCurve)
import OpenCascade.Handle (Handle)
import OpenCascade.Internal.Bool (cBoolToBool)
import OpenCascade.GP (Pnt)
import OpenCascade.GP.Internal.Destructors (deletePnt)
import Data.Coerce (coerce)

foreign import capi unsafe "hs_Geom_BSplineCurve.h hs_Geom_BSplineCurve_toHandle" rawToHandle :: Ptr BSplineCurve -> IO (Ptr (Handle BSplineCurve))

toHandle :: Ptr BSplineCurve -> Acquire (Ptr (Handle BSplineCurve))
toHandle curve = mkAcquire (rawToHandle curve) deleteHandleBSplineCurve

foreign import capi unsafe "hs_Geom_BSplineCurve.h hs_Geom_BSplineCurve_nbPoles" rawNbPoles :: Ptr (Handle BSplineCurve) -> IO (CInt)

nbPoles :: Ptr (Handle (BSplineCurve)) -> IO Int 
nbPoles h = fromIntegral <$> rawNbPoles h

foreign import capi unsafe "hs_Geom_BSplineCurve.h hs_Geom_BSplineCurve_pole" rawPole :: Ptr (Handle BSplineCurve) -> CInt -> IO (Ptr Pnt)

pole :: Ptr (Handle BSplineCurve) -> Int -> Acquire (Ptr Pnt)
pole h n = mkAcquire (rawPole h (fromIntegral n)) deletePnt

foreign import capi unsafe "hs_Geom_BSplineCurve.h hs_Geom_BSplineCurve_isRational" rawIsRational :: Ptr (Handle BSplineCurve) -> IO (CBool)

isRational :: Ptr (Handle (BSplineCurve)) -> IO Bool
isRational h = cBoolToBool <$> rawIsRational h

foreign import capi unsafe "hs_Geom_BSplineCurve.h hs_Geom_BSplineCurve_segment" rawSegment :: Ptr (Handle BSplineCurve) -> CDouble -> CDouble -> CDouble -> IO ()

segment :: Ptr (Handle BSplineCurve) -> Double -> Double -> Double -> IO ()
segment = coerce rawSegment


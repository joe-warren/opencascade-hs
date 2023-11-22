{-# LANGUAGE CApiFFI #-}
module OpenCascade.GC.MakeArcOfCircle
( from3Pnts
, fromPntsAndVec
) where

import qualified OpenCascade.Geom.Internal.Destructors as Geom.Destructors
import qualified OpenCascade.GP as GP
import qualified OpenCascade.Geom as Geom
import OpenCascade.Handle
import Foreign.Ptr
import Data.Acquire 


-- from3Pnts

foreign import capi unsafe "hs_GC_MakeArcOfCircle.h hs_GC_MakeArcOfCircle_from3Pnts" rawFrom3Pnts :: Ptr GP.Pnt -> Ptr GP.Pnt -> Ptr GP.Pnt -> IO (Ptr (Handle (Geom.TrimmedCurve)))

from3Pnts :: Ptr GP.Pnt -> Ptr GP.Pnt -> Ptr GP.Pnt -> Acquire (Ptr (Handle Geom.TrimmedCurve))
from3Pnts start mid end = mkAcquire (rawFrom3Pnts start mid end) Geom.Destructors.deleteHandleTrimmedCurve


-- fromPntsAndVec

foreign import capi unsafe "hs_GC_MakeArcOfCircle.h hs_GC_MakeArcOfCircle_fromPntsAndVec" rawFromPntsAndVec :: Ptr GP.Pnt -> Ptr GP.Vec -> Ptr GP.Pnt -> IO (Ptr (Handle (Geom.TrimmedCurve)))

fromPntsAndVec :: Ptr GP.Pnt -> Ptr GP.Vec -> Ptr GP.Pnt -> Acquire (Ptr (Handle Geom.TrimmedCurve))
fromPntsAndVec start dir end = mkAcquire (rawFromPntsAndVec start dir end) Geom.Destructors.deleteHandleTrimmedCurve




{-# LANGUAGE CApiFFI #-}
module OpenCascade.GC.MakeArcOfCircle
( from3Pnts
, fromPntsAndVec
) where

import qualified OpenCascade.Geom.Internal.Destructors as Geom.Destructors
import qualified OpenCascade.GP as GP
import qualified OpenCascade.Geom as Geom
import OpenCascade.Handle
import OpenCascade.Internal.Exception (wrapException)
import Foreign.C (CInt)
import Foreign.Ptr
import Data.Acquire


-- from3Pnts

foreign import capi unsafe "hs_GC_MakeArcOfCircle.h hs_GC_MakeArcOfCircle_from3Pnts" rawFrom3Pnts
    :: Ptr GP.Pnt
    -> Ptr GP.Pnt
    -> Ptr GP.Pnt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr (Handle (Geom.TrimmedCurve)))

from3Pnts :: Ptr GP.Pnt -> Ptr GP.Pnt -> Ptr GP.Pnt -> Acquire (Ptr (Handle Geom.TrimmedCurve))
from3Pnts start mid end = mkAcquire (wrapException $ rawFrom3Pnts start mid end) Geom.Destructors.deleteHandleTrimmedCurve


-- fromPntsAndVec

foreign import capi unsafe "hs_GC_MakeArcOfCircle.h hs_GC_MakeArcOfCircle_fromPntsAndVec" rawFromPntsAndVec
    :: Ptr GP.Pnt
    -> Ptr GP.Vec
    -> Ptr GP.Pnt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr (Handle (Geom.TrimmedCurve)))

fromPntsAndVec :: Ptr GP.Pnt -> Ptr GP.Vec -> Ptr GP.Pnt -> Acquire (Ptr (Handle Geom.TrimmedCurve))
fromPntsAndVec start dir end = mkAcquire (wrapException $ rawFromPntsAndVec start dir end) Geom.Destructors.deleteHandleTrimmedCurve




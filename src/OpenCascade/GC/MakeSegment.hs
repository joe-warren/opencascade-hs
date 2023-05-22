{-# LANGUAGE CApiFFI #-}
module OpenCascade.GC.MakeSegment
( fromPnts
) where

import qualified OpenCascade.Geom.Internal.Destructors as Geom.Destructors
import qualified OpenCascade.GP as GP
import qualified OpenCascade.Geom as Geom
import OpenCascade.Handle
import Foreign.Ptr
import Data.Acquire 


-- fromPnts

foreign import capi unsafe "hs_GC_MakeSegment.h hs_GC_MakeSegment_fromPnts" rawFromPnts :: Ptr GP.Pnt -> Ptr GP.Pnt -> IO (Ptr (Handle (Geom.TrimmedCurve)))

fromPnts :: Ptr GP.Pnt -> Ptr GP.Pnt -> Acquire (Ptr (Handle Geom.TrimmedCurve))
fromPnts start end = mkAcquire (rawFromPnts start end) Geom.Destructors.deleteHandleTrimmedCurve

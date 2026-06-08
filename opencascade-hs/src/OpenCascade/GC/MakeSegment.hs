{-# LANGUAGE CApiFFI #-}
module OpenCascade.GC.MakeSegment
( fromPnts
) where

import qualified OpenCascade.Geom.Internal.Destructors as Geom.Destructors
import qualified OpenCascade.GP as GP
import qualified OpenCascade.Geom as Geom
import OpenCascade.Handle
import OpenCascade.Internal.Exception (wrapException)
import Foreign.C (CInt)
import Foreign.Ptr
import Data.Acquire


-- fromPnts

foreign import capi unsafe "hs_GC_MakeSegment.h hs_GC_MakeSegment_fromPnts" rawFromPnts
    :: Ptr GP.Pnt
    -> Ptr GP.Pnt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr (Handle (Geom.TrimmedCurve)))

fromPnts :: Ptr GP.Pnt -> Ptr GP.Pnt -> Acquire (Ptr (Handle Geom.TrimmedCurve))
fromPnts start end = mkAcquire (wrapException $ rawFromPnts start end) Geom.Destructors.deleteHandleTrimmedCurve

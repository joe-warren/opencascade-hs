{-# LANGUAGE CApiFFI #-}
module OpenCascade.GCPnts.Internal.Destructors
( deleteAbscissaPoint
) where

import OpenCascade.GCPnts.Types (AbscissaPoint)
import Foreign.Ptr

foreign import capi unsafe "hs_GCPnts_AbscissaPoint.h hs_delete_GCPnts_AbscissaPoint" deleteAbscissaPoint :: Ptr AbscissaPoint -> IO ()

{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepPrimAPI.MakeTorus
( fromRadii
) where

import OpenCascade.BRepPrimAPI.Types (MakeTorus)
import OpenCascade.BRepPrimAPI.Internal.Destructors (deleteMakeTorus)
import Foreign.C
import Foreign.Ptr
import Data.Acquire
import Data.Coerce (coerce)

-- new

foreign import capi unsafe "hs_BRepPrimAPI_MakeTorus.h hs_new_BRepPrimAPI_MakeTorus_fromRadii" rawFromRadii :: CDouble -> CDouble ->  IO (Ptr MakeTorus)

fromRadii :: Double -> Double -> Acquire (Ptr MakeTorus)
fromRadii major minor = mkAcquire (rawFromRadii (coerce major) (coerce minor)) deleteMakeTorus
{-# LANGUAGE CApiFFI #-}
module OpenCascade.NCollection.Vec3
( Vec3
, newVec3Float
) where

import OpenCascade.NCollection.Types (Vec3)
import OpenCascade.NCollection.Internal.Destructors (deleteVec3Float)
import Foreign.Ptr (Ptr)
import Foreign.C (CFloat (..))
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_NCollection_Vec3.h hs_new_NCollection_Vec3_float" rawNewVec3Float :: CFloat -> CFloat -> CFloat -> IO (Ptr (Vec3 Float))

newVec3Float :: Float -> Float -> Float -> Acquire (Ptr (Vec3 Float))
newVec3Float x y z = mkAcquire (rawNewVec3Float (realToFrac x) (realToFrac y) (realToFrac z)) deleteVec3Float

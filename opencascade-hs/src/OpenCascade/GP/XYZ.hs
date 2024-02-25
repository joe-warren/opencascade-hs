{-# LANGUAGE CApiFFI #-}
module OpenCascade.GP.XYZ 
( XYZ
, newXYZ
, fromDoubles
, setX, setY, setZ
, x, y, z
) where

import OpenCascade.GP.Types (XYZ)
import OpenCascade.GP.Internal.Destructors (deleteXYZ)
import Foreign.Ptr (Ptr)
import Foreign.C (CDouble (..))
import Data.Acquire (Acquire, mkAcquire)
import Data.Coerce (coerce)

foreign import capi unsafe "hs_gp_XYZ.h hs_new_gp_XYZ" rawNewXYZ :: IO (Ptr XYZ)

newXYZ :: Acquire (Ptr XYZ)
newXYZ = mkAcquire rawNewXYZ deleteXYZ

foreign import capi unsafe "hs_gp_XYZ.h hs_new_gp_XYZ_fromDoubles" rawFromDoubles :: CDouble -> CDouble -> CDouble -> IO (Ptr XYZ)

fromDoubles :: Double -> Double -> Double -> Acquire (Ptr XYZ)
fromDoubles x' y' z' = mkAcquire ((coerce rawFromDoubles) x' y' z') (deleteXYZ)

foreign import capi unsafe "hs_gp_XYZ.h hs_gp_XYZ_setX" rawSetX :: Ptr XYZ -> CDouble -> IO ()

setX :: Ptr XYZ -> CDouble -> IO ()
setX = coerce  rawSetX

foreign import capi unsafe "hs_gp_XYZ.h hs_gp_XYZ_setY" rawSetY :: Ptr XYZ -> CDouble -> IO ()

setY :: Ptr XYZ -> CDouble -> IO ()
setY = coerce  rawSetY

foreign import capi unsafe "hs_gp_XYZ.h hs_gp_XYZ_setZ" rawSetZ :: Ptr XYZ -> CDouble -> IO ()

setZ :: Ptr XYZ -> CDouble -> IO ()
setZ = coerce  rawSetZ

foreign import capi unsafe "hs_gp_XYZ.h hs_gp_XYZ_x" rawX :: Ptr XYZ -> IO (CDouble)

x :: Ptr XYZ -> IO CDouble
x = coerce rawX

foreign import capi unsafe "hs_gp_XYZ.h hs_gp_XYZ_y" rawY :: Ptr XYZ -> IO (CDouble)

y:: Ptr XYZ -> IO CDouble
y = coerce rawY

foreign import capi unsafe "hs_gp_XYZ.h hs_gp_XYZ_z" rawZ :: Ptr XYZ -> IO (CDouble)

z :: Ptr XYZ -> IO CDouble
z = coerce rawZ
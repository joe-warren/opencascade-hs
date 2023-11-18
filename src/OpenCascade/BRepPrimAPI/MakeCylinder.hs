{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepPrimAPI.MakeCylinder
( fromRadiusAndHeight
) where

import qualified OpenCascade.GP as GP
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.TopoDS.Internal.Destructors as TopoDS.Destructors
import Foreign.C
import Foreign.Ptr
import Data.Acquire 
import Data.Coerce (coerce)

foreign import capi unsafe "hs_BRepPrimAPI_MakeCylinder.h hs_BRepPrimAPI_MakeCylinder_fromRadiusAndHeight" rawFromRadiusAndHeight :: CDouble -> CDouble -> IO (Ptr TopoDS.Solid)

fromRadiusAndHeight :: Double -> Double -> Acquire (Ptr TopoDS.Solid)
fromRadiusAndHeight r h = mkAcquire (rawFromRadiusAndHeight (coerce r) (coerce h)) (TopoDS.Destructors.deleteShape . castPtr)
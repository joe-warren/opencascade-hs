{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepPrimAPI.MakeSphere
( fromRadius
, fromPntAndRadius
) where

import qualified OpenCascade.GP as GP
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.TopoDS.Internal.Destructors as TopoDS.Destructors
import Foreign.C
import Foreign.Ptr
import Data.Acquire 
import Data.Coerce (coerce)

foreign import capi unsafe "hs_BRepPrimAPI_MakeSphere.h hs_BRepPrimAPI_MakeSphere_fromRadius" rawFromRadius :: CDouble -> IO (Ptr TopoDS.Solid)

fromRadius :: Double -> Acquire (Ptr TopoDS.Solid)
fromRadius r = mkAcquire (rawFromRadius (coerce r)) (TopoDS.Destructors.deleteShape . castPtr)


foreign import capi unsafe "hs_BRepPrimAPI_MakeSphere.h hs_BRepPrimAPI_MakeSphere_fromPntAndRadius" rawFromPntAndRadius :: Ptr GP.Pnt -> CDouble -> IO (Ptr TopoDS.Solid)

fromPntAndRadius :: Ptr GP.Pnt -> Double -> Acquire (Ptr TopoDS.Solid)
fromPntAndRadius center radius = mkAcquire (rawFromPntAndRadius center (coerce radius)) (TopoDS.Destructors.deleteShape . castPtr)

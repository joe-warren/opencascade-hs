{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepPrimAPI.MakeCylinder
( fromRadiusAndHeight
) where

import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.TopoDS.Internal.Destructors as TopoDS.Destructors
import OpenCascade.Internal.Exception (wrapException)
import Foreign.C
import Foreign.Ptr
import Data.Acquire
import Data.Coerce (coerce)

foreign import capi unsafe "hs_BRepPrimAPI_MakeCylinder.h hs_BRepPrimAPI_MakeCylinder_fromRadiusAndHeight" rawFromRadiusAndHeight
    :: CDouble
    -> CDouble
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Solid)

fromRadiusAndHeight :: Double -> Double -> Acquire (Ptr TopoDS.Solid)
fromRadiusAndHeight r h = mkAcquire (wrapException $ rawFromRadiusAndHeight (coerce r) (coerce h)) (TopoDS.Destructors.deleteShape . castPtr)
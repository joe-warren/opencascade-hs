{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepPrimAPI.MakeCone
( fromTwoRadiiAndHeight
) where

import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.TopoDS.Internal.Destructors as TopoDS.Destructors
import Foreign.C
import Foreign.Ptr
import Data.Acquire 
import Data.Coerce (coerce)

foreign import capi unsafe "hs_BRepPrimAPI_MakeCone.h hs_BRepPrimAPI_MakeCone_fromTwoRadiiAndHeight" rawFromTwoRadiiAndHeight :: CDouble -> CDouble -> CDouble -> IO (Ptr TopoDS.Solid)

fromTwoRadiiAndHeight :: Double -> Double -> Double -> Acquire (Ptr TopoDS.Solid)
fromTwoRadiiAndHeight r1 r2 h = mkAcquire ((coerce rawFromTwoRadiiAndHeight) r1 r2 h) (TopoDS.Destructors.deleteShape . castPtr)
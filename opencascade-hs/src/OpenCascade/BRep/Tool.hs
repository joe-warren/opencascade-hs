{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRep.Tool
( curve
) where

import qualified OpenCascade.Geom as Geom
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.Handle (Handle)
import OpenCascade.Geom.Internal.Destructors (deleteHandleCurve)
import Foreign.Ptr
import Foreign.C
import Data.Coerce
import Data.Acquire
import qualified Data.Acquire as Data

foreign import capi unsafe "hs_BRep_Tool.h hs_BRep_Tool_curve" rawCurve :: Ptr (TopoDS.Edge) -> CDouble -> CDouble -> IO(Ptr (Handle Geom.Curve))

curve :: Ptr TopoDS.Edge -> Double -> Double -> Data.Acquire (Ptr (Handle Geom.Curve))
curve edge start end = mkAcquire ((coerce rawCurve) edge start end) deleteHandleCurve


{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRep.Tool
( curve
, curveParamFirst
, curveParamLast
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

foreign import capi unsafe "hs_BRep_Tool.h hs_BRep_Tool_curve" rawCurve :: Ptr (TopoDS.Edge) -> IO(Ptr (Handle Geom.Curve))

curve :: Ptr TopoDS.Edge -> Data.Acquire (Ptr (Handle Geom.Curve))
curve edge = mkAcquire (rawCurve edge) deleteHandleCurve

foreign import capi unsafe "hs_BRep_Tool.h hs_BRep_Tool_curveParamFirst" rawCurveParamFirst :: Ptr (TopoDS.Edge) -> IO(CDouble)

curveParamFirst :: Ptr TopoDS.Edge -> IO Double 
curveParamFirst = coerce rawCurveParamFirst

foreign import capi unsafe "hs_BRep_Tool.h hs_BRep_Tool_curveParamLast" rawCurveParamLast :: Ptr (TopoDS.Edge) -> IO(CDouble)

curveParamLast :: Ptr TopoDS.Edge -> IO Double 
curveParamLast = coerce rawCurveParamLast


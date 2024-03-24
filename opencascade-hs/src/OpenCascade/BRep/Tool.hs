{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRep.Tool
( curve
, curveParamFirst
, curveParamLast
, triangulation
) where

import qualified OpenCascade.Geom as Geom
import qualified OpenCascade.TopoDS as TopoDS

import qualified OpenCascade.TopLoc.Types as TopLoc
import qualified OpenCascade.Poly.Types as Poly
import OpenCascade.Poly.Internal.Destructors (deleteHandleTriangulation)
import OpenCascade.Handle (Handle)
import OpenCascade.Geom.Internal.Destructors (deleteHandleCurve)
import Foreign.Ptr
import Foreign.C
import Data.Coerce
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_BRep_Tool.h hs_BRep_Tool_curve" rawCurve :: Ptr (TopoDS.Edge) -> IO(Ptr (Handle Geom.Curve))

curve :: Ptr TopoDS.Edge -> Acquire (Ptr (Handle Geom.Curve))
curve edge = mkAcquire (rawCurve edge) deleteHandleCurve

foreign import capi unsafe "hs_BRep_Tool.h hs_BRep_Tool_curveParamFirst" rawCurveParamFirst :: Ptr (TopoDS.Edge) -> IO(CDouble)

curveParamFirst :: Ptr TopoDS.Edge -> IO Double 
curveParamFirst = coerce rawCurveParamFirst

foreign import capi unsafe "hs_BRep_Tool.h hs_BRep_Tool_curveParamLast" rawCurveParamLast :: Ptr (TopoDS.Edge) -> IO(CDouble)

curveParamLast :: Ptr TopoDS.Edge -> IO Double 
curveParamLast = coerce rawCurveParamLast

foreign import capi unsafe "hs_BRep_Tool.h hs_BRep_Tool_triangulation" rawTriangulation :: Ptr (TopoDS.Face) -> Ptr TopLoc.Location -> IO(Ptr (Handle Poly.Triangulation))

triangulation :: Ptr TopoDS.Face -> Ptr TopLoc.Location -> Acquire (Ptr (Handle Poly.Triangulation))
triangulation face loc = mkAcquire (rawTriangulation face loc) deleteHandleTriangulation
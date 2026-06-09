{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRep.Tool
( curve
, curveParamFirst
, curveParamLast
, pnt
, triangulation
) where

import qualified OpenCascade.Geom as Geom
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.GP as GP

import qualified OpenCascade.TopLoc.Types as TopLoc
import qualified OpenCascade.Poly.Types as Poly
import OpenCascade.Poly.Internal.Destructors (deleteHandleTriangulation)
import OpenCascade.Handle (Handle)
import OpenCascade.Geom.Internal.Destructors (deleteHandleCurve)
import OpenCascade.GP.Internal.Destructors (deletePnt)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.Ptr
import Foreign.C
import Data.Coerce
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_BRep_Tool.h hs_BRep_Tool_curve" rawCurve
    :: Ptr (TopoDS.Edge)
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr (Handle Geom.Curve))

curve :: Ptr TopoDS.Edge -> Acquire (Ptr (Handle Geom.Curve))
curve edge = mkAcquire (wrapException $ rawCurve edge) deleteHandleCurve

foreign import capi unsafe "hs_BRep_Tool.h hs_BRep_Tool_curveParamFirst" rawCurveParamFirst
    :: Ptr (TopoDS.Edge)
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (CDouble)

curveParamFirst :: Ptr TopoDS.Edge -> IO Double
curveParamFirst edge = coerce <$> wrapException (rawCurveParamFirst edge)

foreign import capi unsafe "hs_BRep_Tool.h hs_BRep_Tool_curveParamLast" rawCurveParamLast
    :: Ptr (TopoDS.Edge)
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (CDouble)

curveParamLast :: Ptr TopoDS.Edge -> IO Double
curveParamLast edge = coerce <$> wrapException (rawCurveParamLast edge)

foreign import capi unsafe "hs_BRep_Tool.h hs_BRep_Tool_pnt" rawPnt
    :: Ptr (TopoDS.Vertex)
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr GP.Pnt)

pnt :: Ptr TopoDS.Vertex -> Acquire (Ptr GP.Pnt)
pnt v = mkAcquire (wrapException $ rawPnt v) deletePnt

foreign import capi unsafe "hs_BRep_Tool.h hs_BRep_Tool_triangulation" rawTriangulation
    :: Ptr (TopoDS.Face)
    -> Ptr TopLoc.Location
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr (Handle Poly.Triangulation))

triangulation :: Ptr TopoDS.Face -> Ptr TopLoc.Location -> Acquire (Ptr (Handle Poly.Triangulation))
triangulation face loc = mkAcquire (wrapException $ rawTriangulation face loc) deleteHandleTriangulation
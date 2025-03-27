{-# LANGUAGE CApiFFI #-}
module OpenCascade.GeomConvert.BSplineCurveToBezierCurve
( BSplineCurveToBezierCurve
, fromHandle 
, fromHandleParametersAndTolerance
, nbArcs
, arc
) where

import OpenCascade.GeomConvert.Types (BSplineCurveToBezierCurve)
import OpenCascade.GeomConvert.Internal.Destructors (deleteBSplineCurveToBezierCurve)
import OpenCascade.Geom.Internal.Destructors (deleteHandleBezierCurve)
import qualified OpenCascade.Geom.Types as Geom
import OpenCascade.Handle (Handle)
import Foreign.Ptr (Ptr)
import Foreign.C (CInt (..), CDouble (..))
import Data.Coerce (coerce)
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_GeomConvert_BSplineCurveToBezierCurve.h hs_new_GeomConvert_BSplineCurveToBezierCurve_fromHandle" rawFromHandle :: Ptr (Handle Geom.BSplineCurve) -> IO(Ptr BSplineCurveToBezierCurve)

fromHandle :: Ptr (Handle Geom.BSplineCurve) -> Acquire (Ptr BSplineCurveToBezierCurve)
fromHandle h = mkAcquire (rawFromHandle h) (deleteBSplineCurveToBezierCurve)

foreign import capi unsafe "hs_GeomConvert_BSplineCurveToBezierCurve.h hs_new_GeomConvert_BSplineCurveToBezierCurve_fromHandleParametersAndTolerance" rawFromHandleParametersAndTolerance :: Ptr (Handle Geom.BSplineCurve) -> CDouble -> CDouble -> CDouble -> IO(Ptr BSplineCurveToBezierCurve)

fromHandleParametersAndTolerance :: Ptr (Handle Geom.BSplineCurve) -> Double -> Double -> Double -> Acquire (Ptr BSplineCurveToBezierCurve)
fromHandleParametersAndTolerance h firstParam secondParam tolerance= mkAcquire (rawFromHandleParametersAndTolerance h (coerce firstParam) (coerce secondParam) (coerce tolerance)) (deleteBSplineCurveToBezierCurve)

foreign import capi unsafe "hs_GeomConvert_BSplineCurveToBezierCurve.h hs_GeomConvert_BSplineCurveToBezierCurve_nbArcs" rawNbArcs :: Ptr (BSplineCurveToBezierCurve) -> IO CInt

nbArcs :: Ptr BSplineCurveToBezierCurve -> IO Int
nbArcs p = fromIntegral <$> rawNbArcs p

foreign import capi unsafe "hs_GeomConvert_BSplineCurveToBezierCurve.h hs_GeomConvert_BSplineCurveToBezierCurve_arc" rawArc :: Ptr (BSplineCurveToBezierCurve) -> CInt -> IO (Ptr (Handle Geom.BezierCurve))

arc :: Ptr BSplineCurveToBezierCurve -> Int -> Acquire (Ptr (Handle Geom.BezierCurve))
arc p n = mkAcquire (rawArc p (fromIntegral n)) deleteHandleBezierCurve
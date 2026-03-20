{-# LANGUAGE CApiFFI #-}
module OpenCascade.GCPnts.AbscissaPoint
( AbscissaPoint
, fromCurveAbscissaAndParam
, parameter
, isDone
) where

import OpenCascade.GCPnts.Types (AbscissaPoint)
import OpenCascade.GCPnts.Internal.Destructors (deleteAbscissaPoint)
import qualified OpenCascade.BRepAdaptor.Types as BRepAdaptor
import OpenCascade.Internal.Bool (cBoolToBool)
import Foreign.Ptr
import Foreign.C
import Data.Acquire (Acquire, mkAcquire)
import Data.Coerce (coerce)

foreign import capi unsafe "hs_GCPnts_AbscissaPoint.h hs_new_GCPnts_AbscissaPoint" rawNew :: Ptr BRepAdaptor.Curve -> CDouble -> CDouble -> IO (Ptr AbscissaPoint)

fromCurveAbscissaAndParam :: Ptr BRepAdaptor.Curve -> Double -> Double -> Acquire (Ptr AbscissaPoint)
fromCurveAbscissaAndParam curve abscissa u0 = mkAcquire (rawNew curve (CDouble abscissa) (CDouble u0)) deleteAbscissaPoint

foreign import capi unsafe "hs_GCPnts_AbscissaPoint.h hs_GCPnts_AbscissaPoint_parameter" rawParameter :: Ptr AbscissaPoint -> IO CDouble

parameter :: Ptr AbscissaPoint -> IO Double
parameter = coerce rawParameter

foreign import capi unsafe "hs_GCPnts_AbscissaPoint.h hs_GCPnts_AbscissaPoint_isDone" rawIsDone :: Ptr AbscissaPoint -> IO CBool

isDone :: Ptr AbscissaPoint -> IO Bool
isDone = fmap cBoolToBool . rawIsDone

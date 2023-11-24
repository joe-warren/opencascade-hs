{-# LANGUAGE CApiFFI #-}
module OpenCascade.Geom.Internal.Destructors where

import OpenCascade.Geom.Types

import OpenCascade.Handle

import Foreign.Ptr

foreign import capi unsafe "hs_Geom_Curve.h hs_delete_Handle_Geom_Curve" deleteHandleCurve :: Ptr (Handle Curve) -> IO ()

foreign import capi unsafe "hs_Geom_TrimmedCurve.h hs_delete_Handle_Geom_TrimmedCurve" deleteHandleTrimmedCurve :: Ptr (Handle TrimmedCurve) -> IO ()

foreign import capi unsafe "hs_Geom_BezierCurve.h hs_delete_Handle_Geom_BezierCurve" deleteHandleBezierCurve :: Ptr (Handle BezierCurve) -> IO ()

foreign import capi unsafe "hs_Geom_BezierCurve.h hs_delete_Geom_BezierCurve" deleteBezierCurve :: Ptr BezierCurve -> IO ()

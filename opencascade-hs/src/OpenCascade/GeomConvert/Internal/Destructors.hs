{-# LANGUAGE CApiFFI #-}
module OpenCascade.GeomConvert.Internal.Destructors 
( deleteBSplineCurveToBezierCurve
, deleteApproxCurve
) where

import OpenCascade.GeomConvert.Types

import Foreign.Ptr

foreign import capi unsafe "hs_GeomConvert_BSplineCurveToBezierCurve.h hs_delete_GeomConvert_BSplineCurveToBezierCurve" deleteBSplineCurveToBezierCurve :: Ptr BSplineCurveToBezierCurve -> IO ()
foreign import capi unsafe "hs_GeomConvert_ApproxCurve.h hs_delete_GeomConvert_ApproxCurve" deleteApproxCurve :: Ptr ApproxCurve -> IO ()
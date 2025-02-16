{-# LANGUAGE CApiFFI #-}
module OpenCascade.GeomConvert.Internal.Destructors 
( deleteBSplineCurveToBezierCurve
) where

import OpenCascade.GeomConvert.Types

import Foreign.Ptr

foreign import capi unsafe "hs_GeomConvert_BSplineCurveToBezierCurve.h hs_delete_GeomConvert_BSplineCurveToBezierCurve" deleteBSplineCurveToBezierCurve :: Ptr BSplineCurveToBezierCurve -> IO ()
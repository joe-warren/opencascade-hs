{-# LANGUAGE CApiFFI #-}
module OpenCascade.ShapeConstruct.Internal.Destructors
( deleteCurve
) where

import OpenCascade.ShapeConstruct.Types
import Foreign.Ptr 

foreign import capi unsafe "hs_ShapeConstruct_Curve.h hs_delete_ShapeConstruct_Curve" deleteCurve :: Ptr Curve -> IO ()
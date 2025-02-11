{-# LANGUAGE CApiFFI #-} 
module OpenCascade.BRepAdaptor.Internal.Destructors
( deleteCurve
) where

import OpenCascade.BRepAdaptor.Types (Curve)
import Foreign.Ptr

foreign import capi unsafe "hs_BRepAdaptor_Curve.h hs_delete_BRepAdaptor_Curve" deleteCurve :: Ptr Curve -> IO ()


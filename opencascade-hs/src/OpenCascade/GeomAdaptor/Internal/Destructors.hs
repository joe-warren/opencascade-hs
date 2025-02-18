{-# LANGUAGE CApiFFI #-} 
module OpenCascade.GeomAdaptor.Internal.Destructors
( deleteCurve
) where

import OpenCascade.GeomAdaptor.Types (Curve)
import Foreign.Ptr

foreign import capi unsafe "hs_GeomAdaptor_Curve.h hs_delete_GeomAdaptor_Curve" deleteCurve :: Ptr Curve -> IO ()


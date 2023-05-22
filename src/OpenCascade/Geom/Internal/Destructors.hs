{-# LANGUAGE CApiFFI #-}
module OpenCascade.Geom.Internal.Destructors where

import OpenCascade.Geom.Types

import OpenCascade.Handle

import Foreign.Ptr

foreign import capi unsafe "hs_Geom_TrimmedCurve.h hs_delete_Handle_Geom_TrimmedCurve" deleteHandleTrimmedCurve :: Ptr (Handle TrimmedCurve) -> IO ()



{-# LANGUAGE CApiFFI #-}
module OpenCascade.STEPCAFControl.Internal.Destructors
( deleteWriter
) where

import OpenCascade.STEPCAFControl.Types

import Foreign.Ptr

foreign import capi unsafe "hs_STEPCAFControl_Writer.h hs_delete_STEPCAFControl_Writer" deleteWriter :: Ptr Writer -> IO ()

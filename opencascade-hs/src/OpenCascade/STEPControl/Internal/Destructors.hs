{-# LANGUAGE CApiFFI #-}
module OpenCascade.STEPControl.Internal.Destructors 
( deleteWriter
, deleteReader
) where

import OpenCascade.STEPControl.Types

import Foreign.Ptr

foreign import capi unsafe "hs_STEPControl_Writer.h hs_delete_STEPControl_Writer" deleteWriter :: Ptr Writer -> IO ()
foreign import capi unsafe "hs_STEPControl_Reader.h hs_delete_STEPControl_Reader" deleteReader :: Ptr Reader -> IO ()
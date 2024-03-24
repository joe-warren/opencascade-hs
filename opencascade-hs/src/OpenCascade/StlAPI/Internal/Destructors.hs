{-# LANGUAGE CApiFFI #-}
module OpenCascade.StlAPI.Internal.Destructors 
( deleteWriter
, deleteReader
) where

import OpenCascade.StlAPI.Types

import Foreign.Ptr

foreign import capi unsafe "hs_StlAPI_Writer.h hs_delete_StlAPI_Writer" deleteWriter :: Ptr Writer -> IO ()
foreign import capi unsafe "hs_StlAPI_Reader.h hs_delete_StlAPI_Reader" deleteReader :: Ptr Reader -> IO ()
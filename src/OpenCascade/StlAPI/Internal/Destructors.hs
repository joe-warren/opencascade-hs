{-# LANGUAGE CApiFFI #-}
module OpenCascade.StlAPI.Internal.Destructors where

import OpenCascade.StlAPI.Types

import Foreign.Ptr

foreign import capi unsafe "hs_StlAPI_Writer.h hs_delete_StlAPI_Writer" deleteWriter :: Ptr Writer -> IO ()
{-# LANGUAGE CApiFFI #-}
module OpenCascade.TopLoc.Internal.Destructors where

import OpenCascade.TopLoc.Types

import Foreign.Ptr

foreign import capi unsafe "hs_TopLoc_Location.h hs_delete_TopLoc_Location" deleteLocation :: Ptr Location -> IO ()

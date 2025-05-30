{-# LANGUAGE CApiFFI #-}
module OpenCascade.BOPAlgo.Internal.Destructors 
( deleteBuilder
) where

import OpenCascade.BOPAlgo.Types
import Foreign.Ptr

foreign import capi unsafe "hs_BOPAlgo_Builder.h hs_delete_BOPAlgo_Builder" deleteBuilder :: Ptr Builder -> IO ()

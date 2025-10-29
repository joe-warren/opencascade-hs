{-# LANGUAGE CApiFFI #-}
module OpenCascade.BOPAlgo.Internal.Destructors 
( deleteBuilder
, deleteBOP
) where

import OpenCascade.BOPAlgo.Types
import Foreign.Ptr

foreign import capi unsafe "hs_BOPAlgo_Builder.h hs_delete_BOPAlgo_Builder" deleteBuilder :: Ptr Builder -> IO ()
foreign import capi unsafe "hs_BOPAlgo_BOP.h hs_delete_BOPAlgo_BOP" deleteBOP :: Ptr BOP -> IO ()

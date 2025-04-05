{-# LANGUAGE CApiFFI #-}
module OpenCascade.HLRAlgo.Internal.Destructors 
( deleteProjector
) where

import OpenCascade.HLRAlgo.Types


import Foreign.Ptr

foreign import capi unsafe "hs_HLRAlgo_Projector.h hs_delete_HLRAlgo_Projector" deleteProjector :: Ptr Projector -> IO ()


{-# LANGUAGE CApiFFI #-}
module OpenCascade.HLRBRep.Internal.Destructors 
( deleteAlgo
, deleteHLRToShape
) where

import OpenCascade.HLRBRep.Types
import OpenCascade.Handle

import Foreign.Ptr

foreign import capi unsafe "hs_HLRBRep_Algo.h hs_delete_HLRBRep_Algo" deleteAlgo :: Ptr (Handle Algo) -> IO ()
foreign import capi unsafe "hs_HLRBRep_HLRToShape.h hs_delete_HLRBRep_HLRToShape" deleteHLRToShape :: Ptr HLRToShape -> IO ()

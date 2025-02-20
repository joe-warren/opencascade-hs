{-# LANGUAGE CApiFFI #-}
module OpenCascade.HLRBRep.Internal.Destructors 
( deleteAlgo
, deleteHandleAlgo
) where

import OpenCascade.HLRBRep.Types
import OpenCascade.Handle

import Foreign.Ptr

foreign import capi unsafe "hs_HLRBRep_Algo.h hs_delete_HLRBRep_Algo" deleteAlgo :: Ptr Algo -> IO ()
foreign import capi unsafe "hs_HLRBRep_Algo.h hs_delete_Handle_HLRBRep_Algo" deleteHandleAlgo :: Ptr (Handle Algo) -> IO ()


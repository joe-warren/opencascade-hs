{-# LANGUAGE CApiFFI #-}
module OpenCascade.Standard.Internal.Destructors
( deleteFailure
) where

import OpenCascade.Standard.Types
import Foreign.Ptr 

foreign import capi unsafe "hs_Standard_Failure.h hs_delete_Standard_Failure" deleteFailure :: Ptr Failure -> IO ()
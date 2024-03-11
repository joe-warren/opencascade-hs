{-# LANGUAGE CApiFFI #-}
module OpenCascade.Message.Internal.Destructors
( deleteProgressRange
) where

import OpenCascade.Message.Types
import Foreign.Ptr 

foreign import capi unsafe "hs_Message_ProgressRange.h hs_delete_Message_ProgressRange" deleteProgressRange :: Ptr ProgressRange -> IO ()
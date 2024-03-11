{-# LANGUAGE CApiFFI #-}
module OpenCascade.Message.ProgressRange
( ProgressRange
, new
) where

import OpenCascade.Message.Types (ProgressRange)
import OpenCascade.Message.Internal.Destructors (deleteProgressRange)
import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_Message_ProgressRange.h hs_new_Message_ProgressRange" rawNew :: IO (Ptr ProgressRange)

new :: Acquire (Ptr ProgressRange)
new = mkAcquire rawNew deleteProgressRange